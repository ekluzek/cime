"""
Interface to the env_batch.xml file.  This class inherits from EnvBase
"""

from CIME.XML.standard_module_setup import *
from CIME.XML.env_base import EnvBase
from CIME.utils import transform_vars, get_cime_root, convert_to_seconds, format_time, get_cime_config, get_batch_script_for_job

from collections import OrderedDict
import stat, re, math

logger = logging.getLogger(__name__)

# pragma pylint: disable=attribute-defined-outside-init

class EnvBatch(EnvBase):

    def __init__(self, case_root=None, infile="env_batch.xml"):
        """
        initialize an object interface to file env_batch.xml in the case directory
        """
        self._batchtype = None
        # This arbitrary setting should always be overwritten
        self._default_walltime = "00:20:00"
        schema = os.path.join(get_cime_root(), "config", "xml_schemas", "env_batch.xsd")
        super(EnvBatch,self).__init__(case_root, infile, schema=schema)

    # pylint: disable=arguments-differ
    def set_value(self, item, value, subgroup=None, ignore_type=False):
        """
        Override the entry_id set_value function with some special cases for this class
        """
        val = None

        if item == "JOB_WALLCLOCK_TIME":
            #Most systems use %H:%M:%S format for wallclock but LSF
            #uses %H:%M this code corrects the value passed in to be
            #the correct format - if we find we have more exceptions
            #than this we may need to generalize this further
            walltime_format = self.get_value("walltime_format", subgroup=None)
            if walltime_format is not None and walltime_format.count(":") != value.count(":"): # pylint: disable=maybe-no-member
                if value.count(":") == 1:
                    t_spec = "%H:%M"
                elif value.count(":") == 2:
                    t_spec = "%H:%M:%S"
                else:
                    expect(False, "could not interpret format for wallclock time {}".format(value))
                value = format_time(walltime_format, t_spec, value)

        if item == "JOB_QUEUE":
            expect(value in self._get_all_queue_names() or ignore_type,
                   "Unknown Job Queue specified use --force to set")

        # allow the user to set item for all jobs if subgroup is not provided
        if subgroup is None:
            gnodes = self.get_children("group")
            for gnode in gnodes:
                node = self.get_optional_child("entry", {"id":item}, root=gnode)
                if node is not None:
                    self._set_value(node, value, vid=item, ignore_type=ignore_type)
                    val = value
        else:
            group = self.get_optional_child("group", {"id":subgroup})
            if group is not None:
                node = self.get_optional_child("entry", {"id":item}, root=group)
                if node is not None:
                    val = self._set_value(node, value, vid=item, ignore_type=ignore_type)

        return val

    # pylint: disable=arguments-differ
    def get_value(self, item, attribute=None, resolved=True, subgroup="PRIMARY"):
        """
        Must default subgroup to something in order to provide single return value
        """

        value = None
        if subgroup is None:
            node = self.get_optional_child(item, attribute)
            if node is None:
                # this will take the last instance of item listed in all batch_system elements
                bs_nodes = self.get_children("batch_system")
                for bsnode in bs_nodes:
                    cnode = self.get_optional_child(item, attribute, root=bsnode)
                    if cnode is not None:
                        node = cnode

            if node is not None:
                value = self.text(node)
                if resolved:
                    value = self.get_resolved_value(value)
            else:
                value = super(EnvBatch, self).get_value(item,attribute,resolved)

        else:
            if subgroup == "PRIMARY":
                subgroup = "case.test" if "case.test" in self.get_jobs() else "case.run"
            #pylint: disable=assignment-from-none
            value = super(EnvBatch, self).get_value(item, attribute=attribute, resolved=resolved, subgroup=subgroup)

        return value

    def get_type_info(self, vid):
        gnodes = self.get_children("group")
        for gnode in gnodes:
            nodes = self.get_children("entry",{"id":vid}, root=gnode)
            type_info = None
            for node in nodes:
                new_type_info = self._get_type_info(node)
                if type_info is None:
                    type_info = new_type_info
                else:
                    expect( type_info == new_type_info,
                            "Inconsistent type_info for entry id={} {} {}".format(vid, new_type_info, type_info))
        return type_info

    def get_jobs(self):
        groups = self.get_children("group")
        results = []
        for group in groups:
            if self.get(group, "id") not in ["job_submission", "config_batch"]:
                results.append(self.get(group, "id"))

        return results

    def create_job_groups(self, batch_jobs, is_test):
        # Subtle: in order to support dynamic batch jobs, we need to remove the
        # job_submission group and replace with job-based groups

        orig_group = self.get_child("group", {"id":"job_submission"},
                                    err_msg="Looks like job groups have already been created")
        orig_group_children = super(EnvBatch, self).get_children(root=orig_group)

        childnodes = []
        for child in reversed(orig_group_children):
            childnodes.append(child)

        self.remove_child(orig_group)

        for name, jdict in batch_jobs:
            if name == "case.run" and is_test:
                pass # skip
            elif name == "case.test" and not is_test:
                pass # skip
            elif name == "case.run.sh":
                pass # skip
            else:
                new_job_group = self.make_child("group", {"id":name})
                for field in jdict.keys():
                    val = jdict[field]
                    node = self.make_child("entry", {"id":field,"value":val}, root=new_job_group)
                    self.make_child("type", root=node, text="char")

                for child in childnodes:
                    self.add_child(self.copy(child), root=new_job_group)

    def cleanupnode(self, node):
        if self.get(node, "id") == "batch_system":
            fnode = self.get_child(name="file", root=node)
            self.remove_child(fnode, root=node)
            gnode = self.get_child(name="group", root=node)
            self.remove_child(gnode, root=node)
            vnode = self.get_optional_child(name="values", root=node)
            if vnode is not None:
                self.remove_child(vnode, root=node)
        else:
            node = super(EnvBatch, self).cleanupnode(node)
        return node

    def set_batch_system(self, batchobj, batch_system_type=None):
        if batch_system_type is not None:
            self.set_batch_system_type(batch_system_type)

        if batchobj.batch_system_node is not None and batchobj.machine_node is not None:
            for node in batchobj.get_children("",root=batchobj.machine_node):
                name = self.name(node)
                if name != 'directives':
                    oldnode = batchobj.get_optional_child(name, root=batchobj.batch_system_node)
                    if oldnode is not None:
                        logger.debug( "Replacing {}".format(self.name(oldnode)))
                        batchobj.remove_child(oldnode, root=batchobj.batch_system_node)

        if batchobj.batch_system_node is not None:
            self.add_child(self.copy(batchobj.batch_system_node))
        if batchobj.machine_node is not None:
            self.add_child(self.copy(batchobj.machine_node))
        self.set_value("BATCH_SYSTEM", batch_system_type)

    def make_batch_script(self, input_template, job, case, outfile=None):
        expect(os.path.exists(input_template), "input file '{}' does not exist".format(input_template))
        task_count = self.get_value("task_count", subgroup=job)
        overrides = {}
        if task_count is not None:
            overrides["total_tasks"] = int(task_count)
            overrides["num_nodes"]   = int(math.ceil(float(task_count)/float(case.tasks_per_node)))
        else:
            task_count = case.get_value("TOTALPES")*int(case.thread_count)
        if int(task_count) < case.get_value("MAX_TASKS_PER_NODE"):
            overrides["max_tasks_per_node"] = int(task_count)

        overrides["job_id"] = case.get_value("CASE") + os.path.splitext(job)[1]
        if "pleiades" in case.get_value("MACH"):
            # pleiades jobname needs to be limited to 15 chars
            overrides["job_id"] = overrides["job_id"][:15]

        overrides["batchdirectives"] = self.get_batch_directives(case, job, overrides=overrides)
        overrides["mpirun"] = case.get_mpirun_cmd(job=job)
        output_text = transform_vars(open(input_template,"r").read(), case=case, subgroup=job, overrides=overrides)
        output_name = get_batch_script_for_job(job) if outfile is None else outfile
        logger.info("Creating file {}".format(output_name))
        with open(output_name, "w") as fd:
            fd.write(output_text)

        # make sure batch script is exectuble
        os.chmod(output_name, os.stat(output_name).st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

    def set_job_defaults(self, batch_jobs, case):
        if self._batchtype is None:
            self._batchtype = self.get_batch_system_type()

        if self._batchtype == "none":
            return

        known_jobs = self.get_jobs()

        for job, jsect in batch_jobs:
            if job not in known_jobs:
                continue

            walltime    = case.get_value("USER_REQUESTED_WALLTIME", subgroup=job) if case.get_value("USER_REQUESTED_WALLTIME", subgroup=job) else None
            force_queue = case.get_value("USER_REQUESTED_QUEUE", subgroup=job) if case.get_value("USER_REQUESTED_QUEUE", subgroup=job) else None
            logger.info("job is {} USER_REQUESTED_WALLTIME {} USER_REQUESTED_QUEUE {}".format(job, walltime, force_queue))
            task_count = int(jsect["task_count"]) if "task_count" in jsect else case.total_tasks
            walltime = jsect["walltime"] if ("walltime" in jsect and walltime is None) else walltime
            if "task_count" in jsect:
                # job is using custom task_count, need to compute a node_count based on this
                node_count = int(math.ceil(float(task_count)/float(case.tasks_per_node)))
            else:
                node_count = case.num_nodes

            if force_queue:
                if not self.queue_meets_spec(force_queue, node_count, task_count, walltime=walltime, job=job):
                    logger.warning("WARNING: User-requested queue '{}' does not meet requirements for job '{}'".format(force_queue, job))
                    if self.queue_meets_spec(force_queue, node_count, task_count, walltime=None, job=job):
                        if case.get_value("TEST"):
                            walltime = self.get_queue_specs(force_queue)[3]
                            logger.warning("  Using walltime '{}' instead".format(walltime))
                        else:
                            logger.warning("  Continuing with suspect walltime, batch submission may fail")

                queue = force_queue
            else:
                queue = self.select_best_queue(node_count, task_count, walltime=walltime, job=job)
                if queue is None and walltime is not None:
                    # Try to see if walltime was the holdup
                    queue = self.select_best_queue(node_count, task_count, walltime=None, job=job)
                    if queue is not None:
                        # It was, override the walltime if a test, otherwise just warn the user
                        new_walltime = self.get_queue_specs(queue)[3]
                        expect(new_walltime is not None, "Should never make it here")
                        logger.warning("WARNING: Requested walltime '{}' could not be matched by any queue".format(walltime))
                        if case.get_value("TEST"):
                            logger.warning("  Using walltime '{}' instead".format(new_walltime))
                            walltime = new_walltime
                        else:
                            logger.warning("  Continuing with suspect walltime, batch submission may fail")

                if queue is None:
                    logger.warning("WARNING: No queue on this system met the requirements for this job. Falling back to defaults")
                    default_queue_node = self.get_default_queue()
                    queue = self.text(default_queue_node)
                    walltime = self.get_queue_specs(queue)[3]

            specs = self.get_queue_specs(queue)
            if walltime is None:
                # Figure out walltime
                if specs is None:
                    # Queue is unknown, use specs from default queue
                    walltime = self.get(self.get_default_queue(), "walltimemax")
                else:
                    walltime = specs[3]

                walltime = self._default_walltime if walltime is None else walltime # last-chance fallback

            self.set_value("JOB_QUEUE", queue, subgroup=job, ignore_type=specs is None)
            self.set_value("JOB_WALLCLOCK_TIME", walltime, subgroup=job)
            logger.debug("Job {} queue {} walltime {}".format(job, queue, walltime))

    def _match_attribs(self, attribs, case, queue):
        # check for matches with case-vars
        for attrib in attribs:
            if attrib in ["default", "prefix"]:
                # These are not used for matching
                continue

            elif attrib == "queue":
                if not self._match(queue, attribs["queue"]):
                    return False

            else:
                val = case.get_value(attrib.upper())
                expect(val is not None, "Cannot match attrib '%s', case has no value for it" % attrib.upper())
                if not self._match(val, attribs[attrib]):
                    return False

        return True

    def _match(self, my_value, xml_value):
        if xml_value.startswith("!"):
            result = re.match(xml_value[1:],str(my_value)) is None
        elif isinstance(my_value, bool):
            if my_value: result = xml_value == "TRUE"
            else: result = xml_value == "FALSE"
        else:
            result = re.match(xml_value,str(my_value)) is not None

        logger.debug("(env_mach_specific) _match {} {} {}".format(my_value, xml_value, result))
        return result

    def get_batch_directives(self, case, job, overrides=None):
        """
        """
        result = []
        directive_prefix = None

        roots = self.get_children("batch_system")
        queue = self.get_value("JOB_QUEUE", subgroup=job)
        if self._batchtype != "none" and not queue in self._get_all_queue_names():
            qnode = self.get_default_queue()
            queue = self.text(qnode)

        for root in roots:
            if root is not None:
                if directive_prefix is None:
                    directive_prefix = self.get_element_text("batch_directive", root=root)

                dnodes = self.get_children("directives", root=root)
                for dnode in dnodes:
                    nodes = self.get_children("directive", root=dnode)
                    if self._match_attribs(self.attrib(dnode), case, queue):
                        for node in nodes:
                            directive = self.get_resolved_value("" if self.text(node) is None else self.text(node))
                            default = self.get(node, "default")
                            if default is None:
                                directive = transform_vars(directive, case=case, subgroup=job, default=default, overrides=overrides)
                            else:
                                directive = transform_vars(directive, default=default)

                            custom_prefix = self.get(node, "prefix")
                            prefix = directive_prefix if custom_prefix is None else custom_prefix

                            result.append("{}{}".format("" if not prefix else (prefix + " "), directive))

        return "\n".join(result)

    def get_submit_args(self, case, job):
        '''
        return a list of touples (flag, name)
        '''
        submitargs = " "
        bs_nodes = self.get_children("batch_system")
        submit_arg_nodes = []

        for node in bs_nodes:
            sanode = self.get_optional_child("submit_args", root=node)
            if sanode is not None:
                submit_arg_nodes += self.get_children("arg",root=sanode)

        for arg in submit_arg_nodes:
            flag = self.get(arg, "flag")
            name = self.get(arg, "name")
            if self._batchtype == "cobalt" and job == "case.st_archive":
                if flag == "-n":
                    name = 'task_count'
                if flag == "--mode":
                    continue

            if name is None:
                submitargs+=" {}".format(flag)
            else:
                if name.startswith("$"):
                    name = name[1:]

                if '$' in name:
                    # We have a complex expression and must rely on get_resolved_value.
                    # Hopefully, none of the values require subgroup
                    val = case.get_resolved_value(name)
                else:
                    val = case.get_value(name, subgroup=job)

                if val is not None and len(str(val)) > 0 and val != "None":
                    # Try to evaluate val if it contains any whitespace
                    if " " in val:
                        try:
                            rval = eval(val)
                        except:
                            rval = val
                    else:
                        rval = val
                    # need a correction for tasks per node
                    if flag == "-n" and rval<= 0:
                        rval = 1

                    if flag == "-q" and rval == "batch" and case.get_value("MACH") == "blues":
                        # Special case. Do not provide '-q batch' for blues
                        continue

                    if flag.rfind("=", len(flag)-1, len(flag)) >= 0 or\
                       flag.rfind(":", len(flag)-1, len(flag)) >= 0:
                        submitargs+=" {}{}".format(flag,str(rval).strip())
                    else:
                        submitargs+=" {} {}".format(flag,str(rval).strip())

        return submitargs

    def submit_jobs(self, case, no_batch=False, job=None, user_prereq=None, skip_pnl=False,
                    allow_fail=False, resubmit_immediate=False, mail_user=None, mail_type=None,
                    batch_args=None, dry_run=False):
        alljobs = self.get_jobs()
        startindex = 0
        jobs = []
        firstjob = job
        if job is not None:
            expect(job in alljobs, "Do not know about batch job {}".format(job))
            startindex = alljobs.index(job)

        for index, job in enumerate(alljobs):
            logger.debug( "Index {:d} job {} startindex {:d}".format(index, job, startindex))
            if index < startindex:
                continue
            try:
                prereq = self.get_value('prereq', subgroup=job, resolved=False)
                if prereq is None or job == firstjob or (dry_run and prereq == "$BUILD_COMPLETE"):
                    prereq = True
                else:
                    prereq = case.get_resolved_value(prereq)
                    prereq = eval(prereq)
            except:
                expect(False,"Unable to evaluate prereq expression '{}' for job '{}'".format(self.get_value('prereq',subgroup=job), job))

            if prereq:
                jobs.append((job, self.get_value('dependency', subgroup=job)))

            if self._batchtype == "cobalt":
                break

        depid = OrderedDict()
        jobcmds = []

        if resubmit_immediate:
            num_submit = case.get_value("RESUBMIT") + 1
            case.set_value("RESUBMIT", 0)
            if num_submit <= 0:
                num_submit = 1
        else:
            num_submit = 1

        prev_job = None

        for _ in range(num_submit):
            for job, dependency in jobs:
                if dependency is not None:
                    deps = dependency.split()
                else:
                    deps = []
                dep_jobs = []
                if user_prereq is not None:
                    dep_jobs.append(user_prereq)
                for dep in deps:
                    if dep in depid.keys() and depid[dep] is not None:
                        dep_jobs.append(str(depid[dep]))
                if prev_job is not None:
                    dep_jobs.append(prev_job)

                logger.debug("job {} depends on {}".format(job, dep_jobs))
                result = self._submit_single_job(case, job,
                                                 skip_pnl=skip_pnl,
                                                 resubmit_immediate=resubmit_immediate,
                                                 dep_jobs=dep_jobs,
                                                 allow_fail=allow_fail,
                                                 no_batch=no_batch,
                                                 mail_user=mail_user,
                                                 mail_type=mail_type,
                                                 batch_args=batch_args,
                                                 dry_run=dry_run)
                batch_job_id = str(alljobs.index(job)) if dry_run else result
                depid[job] = batch_job_id
                jobcmds.append( (job, result) )
                if self._batchtype == "cobalt":
                    break
            prev_job = batch_job_id


        if dry_run:
            return jobcmds
        else:
            return depid

    @staticmethod
    def _get_supported_args(job, no_batch):
        """
        Returns a map of the supported parameters and their arguments to the given script
        TODO: Maybe let each script define this somewhere?

        >>> EnvBatch._get_supported_args("", False)
        {}
        >>> EnvBatch._get_supported_args("case.test", False)
        {'skip_pnl': '--skip-preview-namelist'}
        >>> EnvBatch._get_supported_args("case.st_archive", True)
        {'resubmit': '--resubmit'}
        """
        supported = {}
        if job in ["case.run", "case.test"]:
            supported["skip_pnl"] = "--skip-preview-namelist"
        if job == "case.run":
            supported["set_continue_run"] = "--completion-sets-continue-run"
        if job in ["case.st_archive", "case.run"]:
            if job == "case.st_archive" and no_batch:
                supported["resubmit"] = "--resubmit"
            else:
                supported["submit_resubmits"] = "--resubmit"
        return supported

    @staticmethod
    def _build_run_args(job, no_batch, **run_args):
        """
        Returns a map of the filtered parameters for the given script,
        as well as the values passed and the equivalent arguments for calling the script

        >>> EnvBatch._build_run_args("case.run", False, skip_pnl=True, cthulu="f'taghn")
        {'skip_pnl': (True, '--skip-preview-namelist')}
        >>> EnvBatch._build_run_args("case.run", False, skip_pnl=False, cthulu="f'taghn")
        {}
        """
        supported_args = EnvBatch._get_supported_args(job, no_batch)
        args = {}
        for arg_name, arg_value in run_args.items():
            if arg_value and (arg_name in supported_args.keys()):
                args[arg_name] = (arg_value, supported_args[arg_name])
        return args

    def _build_run_args_str(self, job, no_batch, **run_args):
        """
        Returns a string of the filtered arguments for the given script,
        based on the arguments passed
        """
        args = self._build_run_args(job, no_batch, **run_args)
        run_args_str = " ".join(param for _, param in args.values())
        if run_args_str is None:
            return ""

        batch_env_flag = self.get_value("batch_env", subgroup=None)
        if not batch_env_flag:
            return run_args_str
        else:
            return "{} ARGS_FOR_SCRIPT=\'{}\'".format(batch_env_flag, run_args_str)

    def _submit_single_job(self, case, job, dep_jobs=None, allow_fail=False,
                           no_batch=False, skip_pnl=False, mail_user=None, mail_type=None,
                           batch_args=None, dry_run=False, resubmit_immediate=False):
        if not dry_run:
            logger.warning("Submit job {}".format(job))
        batch_system = self.get_value("BATCH_SYSTEM", subgroup=None)
        if batch_system is None or batch_system == "none" or no_batch:
            logger.info("Starting job script {}".format(job))
            function_name = job.replace(".", "_")
            if not dry_run:
                args = self._build_run_args(job, True, skip_pnl=skip_pnl, set_continue_run=resubmit_immediate,
                                            submit_resubmits=not resubmit_immediate)
                getattr(case, function_name)(**{k: v for k, (v, _) in args.items()})

            return

        submitargs = self.get_submit_args(case, job)
        args_override = self.get_value("BATCH_COMMAND_FLAGS", subgroup=job)
        if args_override:
            submitargs = args_override

        if dep_jobs is not None and len(dep_jobs) > 0:
            logger.debug("dependencies: {}".format(dep_jobs))
            if allow_fail:
                dep_string = self.get_value("depend_allow_string", subgroup=None)
                if dep_string is None:
                    logger.warning("'depend_allow_string' is not defined for this batch system, " +
                                   "falling back to the 'depend_string'")
                    dep_string = self.get_value("depend_string", subgroup=None)
            else:
                dep_string = self.get_value("depend_string", subgroup=None)
            expect(dep_string is not None, "'depend_string' is not defined for this batch system")

            separator_string = self.get_value("depend_separator", subgroup=None)
            expect(separator_string is not None,"depend_separator string not defined")

            expect("jobid" in dep_string, "depend_string is missing jobid for prerequisite jobs")
            dep_ids_str = str(dep_jobs[0])
            for dep_id in dep_jobs[1:]:
                dep_ids_str += separator_string + str(dep_id)
            dep_string = dep_string.replace("jobid",dep_ids_str.strip()) # pylint: disable=maybe-no-member
            submitargs += " " + dep_string

        if batch_args is not None:
            submitargs += " " + batch_args

        cime_config = get_cime_config()

        if mail_user is None and cime_config.has_option("main", "MAIL_USER"):
            mail_user = cime_config.get("main", "MAIL_USER")

        if mail_user is not None:
            mail_user_flag = self.get_value('batch_mail_flag', subgroup=None)
            if mail_user_flag is not None:
                submitargs += " " + mail_user_flag + " " + mail_user

        if mail_type is None:
            if job == "case.test" and cime_config.has_option("create_test", "MAIL_TYPE"):
                mail_type = cime_config.get("create_test", "MAIL_TYPE")
            elif cime_config.has_option("main", "MAIL_TYPE"):
                mail_type = cime_config.get("main", "MAIL_TYPE")
            else:
                mail_type = self.get_value("batch_mail_default")

            if mail_type:
                mail_type = mail_type.split(",") # pylint: disable=no-member

        if mail_type:
            mail_type_flag = self.get_value("batch_mail_type_flag", subgroup=None)
            if mail_type_flag is not None:
                mail_type_args = []
                for indv_type in mail_type:
                    mail_type_arg = self.get_batch_mail_type(indv_type)
                    mail_type_args.append(mail_type_arg)

                if mail_type_flag == "-m":
                    # hacky, PBS-type systems pass multiple mail-types differently
                    submitargs += " {} {}".format(mail_type_flag, "".join(mail_type_args))
                else:
                    submitargs += " {} {}".format(mail_type_flag, " {} ".format(mail_type_flag).join(mail_type_args))
        batchsubmit = self.get_value("batch_submit", subgroup=None)
        expect(batchsubmit is not None,
               "Unable to determine the correct command for batch submission.")
        batchredirect = self.get_value("batch_redirect", subgroup=None)
        batch_env_flag = self.get_value("batch_env", subgroup=None)
        run_args = self._build_run_args_str(job, False, skip_pnl=skip_pnl, set_continue_run=resubmit_immediate,
                                            submit_resubmits=not resubmit_immediate)
        if batch_env_flag:
            sequence = (batchsubmit, submitargs, run_args, batchredirect, get_batch_script_for_job(job))
        else:
            sequence = (batchsubmit, submitargs, batchredirect, get_batch_script_for_job(job), run_args)

        submitcmd = " ".join(s.strip() for s in sequence if s is not None)

        if dry_run:
            return submitcmd
        else:
            logger.info("Submitting job script {}".format(submitcmd))
            output = run_cmd_no_fail(submitcmd, combine_output=True)
            jobid = self.get_job_id(output)
            logger.info("Submitted job id is {}".format(jobid))
            return jobid

    def get_batch_mail_type(self, mail_type):
        raw =  self.get_value("batch_mail_type", subgroup=None)
        mail_types = [item.strip() for item in raw.split(",")] # pylint: disable=no-member
        idx = ["never", "all", "begin", "end", "fail"].index(mail_type)

        return mail_types[idx] if idx < len(mail_types) else None

    def get_batch_system_type(self):
        nodes = self.get_children("batch_system")
        for node in nodes:
            type_ = self.get(node, "type")
            if type_ is not None:
                self._batchtype = type_
        return self._batchtype

    def set_batch_system_type(self, batchtype):
        self._batchtype = batchtype

    def get_job_id(self, output):
        jobid_pattern = self.get_value("jobid_pattern", subgroup=None)
        expect(jobid_pattern is not None, "Could not find jobid_pattern in env_batch.xml")
        search_match = re.search(jobid_pattern, output)
        expect(search_match is not None,
               "Couldn't match jobid_pattern '{}' within submit output:\n '{}'".format(jobid_pattern, output))
        jobid = search_match.group(1)
        return jobid

    def queue_meets_spec(self, queue, num_nodes, num_tasks, walltime=None, job=None):
        specs = self.get_queue_specs(queue)
        if specs is None:
            logger.warning("WARNING: queue '{}' is unknown to this system".format(queue))
            return True

        nodemin, nodemax, jobname, walltimemax, jobmin, jobmax, strict = specs

        # A job name match automatically meets spec
        if job is not None and jobname is not None:
            return jobname == job

        if nodemin is not None and num_nodes < nodemin or \
           nodemax is not None and num_nodes > nodemax or \
           jobmin is not None  and num_tasks < jobmin or \
           jobmax is not None  and num_tasks > jobmax:
            return False

        if walltime is not None and walltimemax is not None and strict:
            walltime_s = convert_to_seconds(walltime)
            walltimemax_s = convert_to_seconds(walltimemax)
            if walltime_s > walltimemax_s:
                return False

        return True

    def _get_all_queue_names(self):
        all_queues = []
        all_queues = self.get_all_queues()
        # Default queue needs to be first
        all_queues.insert(0, self.get_default_queue())

        queue_names = []
        for queue in all_queues:
            queue_names.append(self.text(queue))

        return queue_names

    def select_best_queue(self, num_nodes, num_tasks, walltime=None, job=None):
        # Make sure to check default queue first.
        qnames = self._get_all_queue_names()
        for qname in qnames:
            if self.queue_meets_spec(qname, num_nodes, num_tasks, walltime=walltime, job=job):
                return qname

        return None

    def get_queue_specs(self, queue):
        """
        Get queue specifications by name.

        Returns (nodemin, nodemax, jobname, walltimemax, jobmin, jobmax, is_strict)
        """
        for queue_node in self.get_all_queues():
            if self.text(queue_node) == queue:
                nodemin = self.get(queue_node, "nodemin")
                nodemin = None if nodemin is None else int(nodemin)
                nodemax = self.get(queue_node, "nodemax")
                nodemax = None if nodemax is None else int(nodemax)

                jobmin = self.get(queue_node, "jobmin")
                jobmin = None if jobmin is None else int(jobmin)
                jobmax = self.get(queue_node, "jobmax")
                jobmax = None if jobmax is None else int(jobmax)

                expect( nodemin is None or jobmin is None, "Cannot specify both nodemin and jobmin for a queue")
                expect( nodemax is None or jobmax is None, "Cannot specify both nodemax and jobmax for a queue")

                jobname = self.get(queue_node, "jobname")
                walltimemax = self.get(queue_node, "walltimemax")
                strict = self.get(queue_node, "strict") == "true"

                return nodemin, nodemax, jobname, walltimemax, jobmin, jobmax, strict

        return None

    def get_default_queue(self):
        bs_nodes = self.get_children("batch_system")
        node = None
        for bsnode in bs_nodes:
            qnodes = self.get_children("queues", root=bsnode)
            for qnode in qnodes:
                node = self.get_optional_child("queue", attributes={"default" : "true"}, root=qnode)
                if node is None:
                    node = self.get_optional_child("queue", root=qnode)

        expect(node is not None, "No queues found")
        return node

    def get_all_queues(self):
        bs_nodes = self.get_children("batch_system")
        nodes = []
        for bsnode in bs_nodes:
            qnode = self.get_optional_child("queues", root=bsnode)
            if qnode is not None:
                nodes.extend(self.get_children("queue", root=qnode))
        return nodes

    def get_children(self, name=None, attributes=None, root=None):
        if name in ("JOB_WALLCLOCK_TIME", "PROJECT", "CHARGE_ACCOUNT",
                        "PROJECT_REQUIRED", "JOB_QUEUE", "BATCH_COMMAND_FLAGS"):
            nodes = super(EnvBatch, self).get_children("entry", attributes={"id":name}, root=root)
        else:
            nodes = super(EnvBatch, self).get_children(name, attributes=attributes, root=root)

        return nodes

    def get_status(self, jobid):
        batch_query = self.get_optional_child("batch_query")
        if batch_query is None:
            logger.warning("Batch queries not supported on this platform")
        else:
            cmd = self.text(batch_query) + " "
            if self.has(batch_query, "per_job_arg"):
                cmd += self.get(batch_query, "per_job_arg") + " "

            cmd += jobid

            status, out, err = run_cmd(cmd)
            if status != 0:
                logger.warning("Batch query command '{}' failed with error '{}'".format(cmd, err))
            else:
                return out.strip()

    def cancel_job(self, jobid):
        batch_cancel = self.get_optional_child("batch_cancel")
        if batch_cancel is None:
            logger.warning("Batch cancellation not supported on this platform")
            return False
        else:
            cmd = self.text(batch_cancel) + " "  + str(jobid)

            status, out, err = run_cmd(cmd)
            if status != 0:
                logger.warning("Batch cancel command '{}' failed with error '{}'".format(cmd, out + "\n" + err))
            else:
                return True

    def compare_xml(self, other):
        xmldiffs = {}
        f1batchnodes = self.get_children("batch_system")
        for bnode in f1batchnodes:
            f2bnodes = other.get_children("batch_system",
                                          attributes = self.attrib(bnode))
            f2bnode=None
            if len(f2bnodes):
                f2bnode = f2bnodes[0]
            f1batchnodes = self.get_children(root=bnode)
            for node in f1batchnodes:
                name = self.name(node)
                text1 = self.text(node)
                text2 = ""
                attribs = self.attrib(node)
                f2matches = other.scan_children(name, attributes=attribs, root=f2bnode)
                foundmatch=False
                for chkmatch in f2matches:
                    name2 = other.name(chkmatch)
                    attribs2 = other.attrib(chkmatch)
                    text2 = other.text(chkmatch)
                    if(name == name2 and attribs==attribs2 and text1==text2):
                        foundmatch=True
                        break
                if not foundmatch:
                    xmldiffs[name] = [text1, text2]

        f1groups = self.get_children("group")
        for node in f1groups:
            group = self.get(node, "id")
            f2group = other.get_child("group", attributes={"id":group})
            xmldiffs.update(super(EnvBatch, self).compare_xml(other,
                                              root=node, otherroot=f2group))
        return xmldiffs

    def make_all_batch_files(self, case):
        machdir  = case.get_value("MACHDIR")
        logger.info("Creating batch scripts")
        for job in self.get_jobs():
            input_batch_script = os.path.join(machdir,self.get_value('template', subgroup=job))
            logger.info("Writing {} script from input template {}".format(job, input_batch_script))
            self.make_batch_script(input_batch_script, job, case)

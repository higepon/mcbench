{application, mcbench,
 [{description, "mcbench memcached benchmark"},
  {vsn, "1"},
  {modules, [mcbench]},
  {registered, [mcbench]},
  {applications, [kernel, stdlib]},
  {mod, {mcbench_app,[]}}
 ]}.

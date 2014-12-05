publishTo :=
  Some(Resolver.ssh(
    "mirkwood releases",
    "mirkwood.informatik.uni-ulm.de",
    "/data/mvn") withPermissions("0644"))

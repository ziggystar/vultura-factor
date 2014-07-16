publishTo :=
  Some(Resolver.ssh(
    "tgeier releases",
    "companion.informatik.uni-ulm.de",
    "/media/SFB-Space/SambaLDAP/HOMES/tgeier/public_html/mvn") withPermissions "0644")
return {
    name = "creationix/lit-sample-docker-app",
    version = "0.0.0",
    private = true,
    dependencies = {
      "luvit/require@2",
      "luvit/pretty-print@2",
      "luvit/coro-fs@2",
      "creationix/weblit-app@3",
      "creationix/weblit-auto-headers@2",
      "creationix/weblit-logger@2"
    },
    files = {"**.lua", "!test*"}
}

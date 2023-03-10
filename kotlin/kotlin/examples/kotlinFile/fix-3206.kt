object o {

    val c = this::class.java.getResource("r")?.openConnection()
    val d = this.c
}

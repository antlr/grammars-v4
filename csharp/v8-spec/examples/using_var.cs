using System.IO;
class C {
    void M() {
        using var r = new StringReader("hello");
    }
}

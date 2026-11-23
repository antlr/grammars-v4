class ForGeneratorIf {
        def main() = {
                for (index <- 0 to 10 if index < 5) {
                        System.out.println(index)
                }
        }
}

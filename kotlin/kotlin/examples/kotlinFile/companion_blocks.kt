// Tests companion block declarations across class-like declarations.

package sample.companionblocks

class CompanionBlockHost {
    companion {}

    companion {
        val answer = 42
        fun compute() = answer
        class Nested {
            companion { fun deep() = 1 }
        }
    }

    private companion { val privateValue = 7 }
    companion object Named { fun regular() = 0 }
    companion fun extension() = 2
}

object ObjectHost {
    companion { fun member() = 3 }
}

enum class EnumHost {
    FIRST;
    companion { fun enumMember() = 4 }
}

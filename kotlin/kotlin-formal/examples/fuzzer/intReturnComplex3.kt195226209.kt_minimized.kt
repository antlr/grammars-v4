public class Holder {

}
public inline fun <R> doCall2_1(block: (() -> R), exception: ((e: (Exception)?) -> Unit), res: R, h: Holder): R {
try {
return doCall2<R>(block, exception, {}, res, h)
}
finally {

}
}
public inline fun <R> doCall2(block: (() -> R), exception: ((e: Exception) -> (Unit)?), finallyBlock: (() -> Unit), res: R, h: Holder): R {
try {
try {

}catch(e: Exception) {

}
finally {
(finallyBlock)!!()
}
}
finally {

}
return res
}
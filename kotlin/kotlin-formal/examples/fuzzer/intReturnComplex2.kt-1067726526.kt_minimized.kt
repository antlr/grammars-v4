public class Holder {

}
public inline fun <R> doCall2_2(block: (() -> R), res: R, h: Holder): R {
try {
return doCall2_1(block, {}, res, h)
}
finally {

}
}
public inline fun <R> doCall2_1(block: (() -> R), exception: ((e: Exception) -> Unit), res: R, h: Holder): R {
try {
return doCall2<R>(block, exception, {}, res, h)
}
finally {

}
}
public inline fun <R> doCall2(block: (() -> R), exception: ((e: Exception) -> Unit), finallyBlock: (() -> Unit), res: R, h: Holder): R {
try {
try {

}catch(e: Exception) {
(exception)!!(e)
}
finally {

}
}
finally {

}
return res
}
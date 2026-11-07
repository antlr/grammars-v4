// Tests file annotations, annotation use-site targets, and type projections.

@file
:
JvmName("AnnotationProjections")
@file:
[
    Suppress("unused")
    Deprecated
]
package sample.annotationprojections

import kotlin.Deprecated

@field class FieldOrdinary
@file.Qualified<Marker>("ordinary") class FileOrdinary
@property<Marker> class PropertyOrdinary
@get("ordinary") class GetOrdinary
@set class SetOrdinary
@receiver class ReceiverOrdinary
@param class ParamOrdinary
@setparam class SetParamOrdinary
@delegate class DelegateOrdinary

@get: GetterMarker
fun targeted() {}

@field:[FieldMarker OtherMarker]
class GroupedTarget

class Box<T>

fun projectionSamples(
    first: Box<@First in String>,
    second: Box<in @Second String>,
    third: Box<@Third @Fourth out @Fifth String>,
    continued: Box<
        @Before
        in
        @After
        String
    >
) {}

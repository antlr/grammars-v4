class ThingTemplate {

}
annotation class ThingVal(template: ThingTemplate) {
val Any.prop = template.prop
}
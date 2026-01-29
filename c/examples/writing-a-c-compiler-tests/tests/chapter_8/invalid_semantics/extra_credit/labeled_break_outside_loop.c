int main(void) {
    // make sure our usual analysis of break/continue labels also traverses labeled statements
    label: break;
    return 0;
}
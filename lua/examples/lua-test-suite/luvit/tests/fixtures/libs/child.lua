print("Load child module")
exports.name = "Child"
exports.parent = require('./parent')
print("Loaded child module")

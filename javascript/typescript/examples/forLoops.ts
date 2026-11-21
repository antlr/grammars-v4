const are = ["Test", "Test2"];

for (let i = 0; i < are.length; i++) {
    console.log(are[i])
}

for (const a in are) {
    console.log(a)
}

for (const a of are) {
    console.log(a)
}
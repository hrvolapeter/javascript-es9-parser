function a() {
    return 1;
    console.log("unreachable");
    return 2;
}
console.log(a());

function b() {
    return;
}
console.log(b());

function c() {
    if (true) {
        return 1;
    }
    return 2;
}
console.log(c());

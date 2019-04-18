if (true) {
    console.log(1);
} else {
    console.log(false);
}

if (true) {
    console.log(2);
}

if (false) {
    console.log(false);
}

function a() {
    return 1;
}

if (a()) {
    if (true) {
        var b = 3;
    }
    console.log(b);
}

if (true || true) console.log(4);

if (1 < 5) {
    console.log(5);
}

if (1 <= 1) {
    console.log(6);
}

if (1 <= 0) {
    console.log(6);
}
function a(b) {
    b();
}

function c() {
    console.log('c');
}
a(c);
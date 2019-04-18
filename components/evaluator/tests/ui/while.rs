var a = 0;
while (a === 0) {
    console.log(1);
    a = 1;
}

function b() {
    while(true) {
        return 2;
    }
}

console.log(b());

var c = 0;
while (c < 4) {
    c += 1;
    console.log(c);
}

var c = 4;
while (c > 0) {
    c -= 1;
    console.log(c);
}

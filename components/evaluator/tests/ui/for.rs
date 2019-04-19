for(var i = 0; i < 4; i += 1) {
    console.log(i);
}

var i = 0;
for(; i < 4; i += 1) {
    console.log(i);
}

var i = 0;
for(;; i += 1) {
    console.log(i);
    if (i >= 3) {
        return;
    }
}

var i = 0;
for(;;) {
    console.log(i);
    i += 1;
    if (i >= 4) {
        return;
    }
}
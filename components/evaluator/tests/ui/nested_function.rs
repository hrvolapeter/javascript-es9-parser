function a() {
    console.log(1);
    function b() {
        console.log(2);
    }
    b();
    console.log(3);
}

a();

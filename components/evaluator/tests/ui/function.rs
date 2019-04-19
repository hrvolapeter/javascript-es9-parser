function a() {
    console.log(1);
}
a();

var a = function() {
    console.log(2);
};
a();

var a = () => {
    console.log(3);
};
a();

var a = (a) => {
    console.log(a);
};
a(4);

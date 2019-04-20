// JAVASCRIPT EXMAPLE
// Just test if it compiles and runs without error, no output checked
// FROM https://github.com/Muzietto/es6-lambda-calculus by Muzietto


var IDENTITY = x => x;
var ID = IDENTITY;
var I = IDENTITY;

console.log(IDENTITY(1) === 1); // --> 1
IDENTITY(IDENTITY); // --> IDENTITY

var ADD_1 = x => x + 1;

ADD_1(12); // --> 13

var SELF_APPLY = s => s(s);
var S = SELF_APPLY;

S(I); // --> I

var APPLY = f => x => f(x);

APPLY(ID)(SELF_APPLY); // ID

var PAIR = x => y => f => f(x)(y);

PAIR(1)(2); // f => f(1)(2)

var FIRST = function (x) {return function (y) {return x;};};

var SECOND = x => y => y;

console.log(PAIR(1)(2)(FIRST) === 1); // --> 1
console.log(PAIR(1)(2)(SECOND) === 2); // --> 2

var COND = x => y => c => c(x)(y);

var MAX = x => y => {if (x>y) {return x;} else {return y;}};

console.log(COND(1)(2)(MAX) === 2);  //--> 2
console.log(COND(11)(2)(MAX) === 11); // --> 11

var TRUE = FIRST; //x => y => x

var FALSE = SECOND;// x => y => y

console.log(COND(1)(2)(TRUE) === 1); //--> 1
console.log(COND(1)(2)(FALSE) === 2); //--> 2

// !x = x ? false: true

var NOT = x => COND(FALSE)(TRUE)(x);

// NOT(TRUE) --> x => y => y
// NOT(FALSE) --> x => y => x

// x && y = x ? y : false

var AND = x => y => COND(y)(FALSE)(x);

// AND(TRUE)(FALSE) --> x => y => y
// AND(TRUE)(TRUE) --> x => y => x

// x || y = x ? true : y

var OR = x => y => COND(TRUE)(y)(x);

// OR(FALSE)(TRUE) --> x => y => x

var _AND = x => y => x(y)(FALSE);
var _OR = x => y => x(TRUE)(y);

var ZERO = IDENTITY;

var ISZERO = n => n(FIRST);

var SUCC = n => PAIR(FALSE)(n);

var ONE   = SUCC(ZERO);
var TWO   = SUCC(ONE);
var THREE = SUCC(TWO);
var FOUR  = SUCC(THREE);
var FIVE  = SUCC(FOUR);
var SIX   = SUCC(FIVE);
var SEVEN = SUCC(SIX);
var EIGHT = SUCC(SEVEN);
var NINE  = SUCC(EIGHT);
var TEN   = SUCC(NINE);

ISZERO(ONE);
ISZERO(SUCC(ZERO));


var PRED = n => ISZERO(n)(ZERO)(n(SECOND));

PRED(ONE); // x => x
PRED(TWO); // f => f(x)(y)
ISZERO(PRED(ONE)); // x => y => x
ISZERO(PRED(TWO)); // x => y => y

// helper function for addition
var ADD2 = f => x => y => ISZERO(y)(x)(f(f)(SUCC(x))(PRED(y)));
var ADD3 = f => x => y => { if (ISZERO(y) === TRUE) {
    return x;
  } else {
    return f(f)(SUCC(x))(PRED(y));
  }
};

// recursive addition
// var ADD = SELF_APPLY(ADD2)

ADD2(ONE)(TWO);

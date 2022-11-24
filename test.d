module Foo;

int foo(string s)(int y) {
    y = y + 1;
    mixin(s ~ "return y;");
}

pragma(msg, foo!"y = y*2;"(3));

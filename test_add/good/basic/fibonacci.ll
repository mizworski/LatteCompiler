declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.main.5 = internal constant [42 x i8] c"Expected a non-negative integer, but got:\00"

define i32 @fibonacci(i32 %n) {
.entry.0:
%n.0 = alloca i32
store i32 %n, i32* %n.0
    br label %.if.cond.1
.if.cond.1:
    %0 = load i32, i32* %n.0
    %1 = icmp sle i32 %0, 1
    br i1 %1, label %.if.true.2, label %.after.cond.3
.if.true.2:
    %2 = load i32, i32* %n.0
    ret i32 %2
.after.cond.3:
    %fib_a.2 = alloca i32
    store i32 0, i32* %fib_a.2
    %fib_b.2 = alloca i32
    store i32 1, i32* %fib_b.2
    %tmp.2 = alloca i32
    store i32 0, i32* %tmp.2
    %i.2 = alloca i32
    store i32 2, i32* %i.2
    br label %.loop.cond.5
.loop.body.4:
    %3 = load i32, i32* %fib_b.2
    %4 = load i32, i32* %fib_a.2
    %5 = add i32 %3, %4
    store i32 %5, i32* %tmp.2
    %6 = load i32, i32* %fib_b.2
    store i32 %6, i32* %fib_a.2
    %7 = load i32, i32* %tmp.2
    store i32 %7, i32* %fib_b.2
    %8 = load i32, i32* %i.2
    %9 = add i32 %8, 1
    store i32 %9, i32* %i.2
    br label %.loop.cond.5
.loop.cond.5:
    %10 = load i32, i32* %i.2
    %11 = load i32, i32* %n.0
    %12 = icmp sle i32 %10, %11
    br i1 %12, label %.loop.body.4, label %.after.loop.6
.after.loop.6:
    %13 = load i32, i32* %fib_b.2
    ret i32 %13
}

define i32 @main() {
.entry.0:
    %i.0 = alloca i32
    %0 = call i32 @readInt()
    store i32 %0, i32* %i.0
    br label %.if.cond.1
.if.cond.1:
    %1 = load i32, i32* %i.0
    %2 = icmp sge i32 %1, 0
    br i1 %2, label %.if.true.2, label %.if.false.3
.if.true.2:
    %3 = load i32, i32* %i.0
    %4 = call i32 @fibonacci(i32 %3)
    call void @printInt(i32 %4)
    ret i32 0
.if.false.3:
    %5 = getelementptr [42 x i8], [42 x i8]* @.str.main.5, i32 0, i32 0
    call void @printString(i8* %5)
    %6 = load i32, i32* %i.0
    call void @printInt(i32 %6)
    ret i32 1
}


declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.print.0 = internal constant [5 x i8] c"ahoj\00"

define i32 @main() {
.entry.0:
    br label %.land.left.expr.1
.land.left.expr.1:
    %0 = call i1 @print()
    %1 = icmp eq i1 %0, 1
    br i1 %1, label %.land.right.expr.2, label %.land.after.3
.land.right.expr.2:
    br label %.land.after.3
.land.after.3:
    %2 = phi i1 [ 0, %.land.left.expr.1 ], [ 0, %.land.right.expr.2 ]
    ret i32 0
}

define i1 @print() {
.entry.0:
    %0 = getelementptr [5 x i8], [5 x i8]* @.str.print.0, i32 0, i32 0
    call void @printString(i8* %0)
    ret i1 1
}


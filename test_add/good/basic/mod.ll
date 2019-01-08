declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)


define i32 @main() {
.entry.0:
    %0 = srem i32 5, 3
    %1 = mul i32 %0, 3
    %2 = icmp slt i32 %1, 0
    br i1 %2, label %.opposite.remainder.1, label %.mod.result.2
.opposite.remainder.1:
    %3 = add i32 %0, 3
    br label %.mod.result.2
.mod.result.2:
    %4 = phi i32 [ %0, %.entry.0 ], [ %3, %.opposite.remainder.1 ]
    call void @printInt(i32 %4)
    %5 = sub i32 0, 5
    %6 = srem i32 %5, 3
    %7 = mul i32 %6, 3
    %8 = icmp slt i32 %7, 0
    br i1 %8, label %.opposite.remainder.3, label %.mod.result.4
.opposite.remainder.3:
    %9 = add i32 %6, 3
    br label %.mod.result.4
.mod.result.4:
    %10 = phi i32 [ %6, %.mod.result.2 ], [ %9, %.opposite.remainder.3 ]
    call void @printInt(i32 %10)
    ret i32 0
}


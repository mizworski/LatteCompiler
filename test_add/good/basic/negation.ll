declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)


define i32 @main() {
.entry.0:
    %0 = sub i32 0, 1
    %1 = sub i32 0, %0
    call void @printInt(i32 %1)
    %i.0 = alloca i32
    store i32 1, i32* %i.0
    %2 = load i32, i32* %i.0
    %3 = sub i32 0, %2
    call void @printInt(i32 %3)
    %4 = load i32, i32* %i.0
    %5 = sub i32 0, %4
    %6 = sub i32 2, %5
    call void @printInt(i32 %6)
    ret i32 0
}


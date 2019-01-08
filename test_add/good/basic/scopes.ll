declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)


define i32 @main() {
.entry.0:
    %i.0 = alloca i32
    store i32 0, i32* %i.0
    %0 = load i32, i32* %i.0
    call void @printInt(i32 %0)
    %i.1 = alloca i32
    store i32 1, i32* %i.1
    %1 = load i32, i32* %i.1
    call void @printInt(i32 %1)
    %2 = load i32, i32* %i.0
    call void @printInt(i32 %2)
    %i.3 = alloca i32
    store i32 2, i32* %i.3
    %3 = load i32, i32* %i.3
    call void @printInt(i32 %3)
    %4 = load i32, i32* %i.0
    call void @printInt(i32 %4)
    ret i32 0
}


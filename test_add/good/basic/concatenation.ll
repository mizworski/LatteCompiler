declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.main.0 = internal constant [2 x i8] c"a\00"
@.str.main.1 = internal constant [2 x i8] c"b\00"

define i32 @main() {
.entry.0:
    %0 = getelementptr [2 x i8], [2 x i8]* @.str.main.0, i32 0, i32 0
    %1 = getelementptr [2 x i8], [2 x i8]* @.str.main.1, i32 0, i32 0
    %2 = call i8* @__concat(i8* %0, i8* %1)
    call void @printString(i8* %2)
    ret i32 0
}


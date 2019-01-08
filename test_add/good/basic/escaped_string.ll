declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.f.4 = internal constant [157 x i8] c"\22\0Apop\0Apowrot:\0Agetstatic java/lang/System/out Ljava/io/PrintStream;\0Aldc \22zle \22\0Ainvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\0Agoto powrot\0Aldc \22\00"

define i32 @f(i32 %p) {
.entry.0:
%p.0 = alloca i32
store i32 %p, i32* %p.0
    %c.0 = alloca i32
    %0 = load i32, i32* %p.0
    %1 = load i32, i32* %p.0
    %2 = mul i32 2, %1
    %3 = add i32 %0, %2
    store i32 %3, i32* %c.0
    %4 = getelementptr [157 x i8], [157 x i8]* @.str.f.4, i32 0, i32 0
    call void @printString(i8* %4)
    %5 = load i32, i32* %c.0
    ret i32 %5
}

define i32 @main() {
.entry.0:
    %0 = call i32 @f(i32 1)
    %1 = sub i32 %0, 3
    ret i32 %1
}


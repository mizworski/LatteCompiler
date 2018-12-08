# LatteCompiler

https://stackoverflow.com/questions/5430700/how-to-get-nice-syntax-error-messages-with-happy

Zakładam że nazw argumentów nie można przesłonić bezpośrednio w ciele funkcji, a dopiero w pierwszym
bloku. Są one traktowane na równi ze zmiennymi zadeklarowanymi bezpośrednio w ciele funkcji.

Pole usedNames w Envie zostało dodane juz po napisaniu funkcji sprawdzających unikalność nazw. 
Z tego powodu to sprawdzenie wykorzystuje mapę do badania unikalności nazw.
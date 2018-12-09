# LatteCompiler

https://stackoverflow.com/questions/5430700/how-to-get-nice-syntax-error-messages-with-happy

Zakładam że nazw argumentów nie można przesłonić bezpośrednio w ciele funkcji, a dopiero w pierwszym
bloku. Są one traktowane na równi ze zmiennymi zadeklarowanymi bezpośrednio w ciele funkcji.

Komunikaty o błędzie wyświetlają się w postaci:

nazwa_pliku:wiersz:kolumna: komunikat

Testowanie przykładów odbywa się za pomocą polecenia `stack test`

Plikiem wykonywalnym jest plik `latc_x86_64`
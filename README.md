# LatteCompiler

https://stackoverflow.com/questions/5430700/how-to-get-nice-syntax-error-messages-with-happy

Zakładam że nazw argumentów nie można przesłonić bezpośrednio w ciele funkcji, a dopiero w pierwszym
bloku. Są one traktowane na równi ze zmiennymi zadeklarowanymi bezpośrednio w ciele funkcji.

Komunikaty o błędzie wyświetlają się w postaci:

nazwa_pliku:wiersz:kolumna: komunikat

Testowanie przykładów odbywa się za pomocą polecenia `stack test`

Plikiem wykonywalnym jest plik `latc_x86_64`

Dla Ifów - jeżeli warunek da rade się uprościć -> usuwam nieużywany kod. 
W pozostałych przypadkach:

Dla If bez else, jeżeli blok może się zakończyć lub się kończy (status Ended lub MaybeEnded), to dany if może się 
zakończyć.

Dla IfElse, jeżeli obydwa bloki się kończą (status Ended), to całość się kończy. Jeżeli chociaż jeden blok może się 
zakończyć (status Ended lub MaybeEnded), to dana instrukcja może się zakończyć. W przeciwnym wypadku 
(jeżel żaden blok się nie kończy, status Running), to dany ifelse ma status running.
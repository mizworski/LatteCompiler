# LatteCompiler

Kompilator języka Latte do kodu LLVM.

Michał Izworski 360968

## Kompilacja

Wykonanie polecenia `make` uruchamia kolejno polecenia `cabal update`, `cabal install` oraz przenosi gotowy plik
wkonywalny do korzenia projektu. Możliwa jest również kompilacja przy użyciu stacka: `make build`

## Używane narzędzia i biblioteki

Dodatkowe funkcje zawarte są w katalogu `lib`, są to funkcje napisane w LLVM, które są funkcjami wbudowanymi w
język Latte. Dodatkowo zaimplementowana została również funkcja `__cocnat`, która wykorzysytwana jest do konkatenacji
stringów.

Wszystkie biblioteki wykorzystywane w projekcie są bibliotekami standardowymi.

## Rozszerzenia

Obecnie kompilator nie posiada żadnych rozszerzeń, są zaimplementowane jedynie podstawowe funkcjonalności języka
Latte.

## Struktura projektu

W korzeniu projektu znajdują się następujące katalogi:

* `app` - zawiera funkcję `Main`
* `dist` - zawiera pliki utworzone podczas budowania oraz instalacji za pomocą `cabal`
* `lib` - zawiera funkcje wbudowane napisane w LLVM
* `test` - katalog zawierający przykładowe poprawne oraz niepoprawne programy, razem z oczekiwanymi wyjściami
* `src` - katalog z plikami źródłowymi, dzielący się na katalogi odpowiadające za backend oraz frontend. W katalogu 
frontendowym zawarta jest gramatyka języka `Latte.cf`, pliki utworzone przy pomocy BNFC, niektóre ręcznie modyfikowane
oraz pliki `.hs`.

W korzeniu znajdują się również następujące pliki:
* `latc_llvm` - kompilator (po zbudowaniu `make`)
* `run_tests.sh` - skrypt wykorzystujący kompilator (`latc_llvm`), uruchamający kompilator na każdym pliku 
w katalogu `test` i sprawdzający otrzymany wynik z oczekiwanym

## Założenia oraz uwagi

Zakładam że nazw argumentów nie można przesłonić bezpośrednio w ciele funkcji, a dopiero w pierwszym
bloku. Są one zatem traktowane jak gdyby zostały zadeklarowane na początku ciała funkcji.

Komunikaty o błędzie wyświetlają się w postaci:
`nazwa_pliku:wiersz:kolumna: komunikat`

Dla Ifów - jeżeli warunek da rade się uprościć -> usuwam nieużywany kod. 
W pozostałych przypadkach:

Zakładam że plik wejściowy ma roszerzenie `.lat`, inaczej kompilator rzuca błąd.

Operacja `%` jest operacją modulus, a nie remainder, tzn. `5 % (-2) == -1` a `-5 % 2 == 1`.

Pętle i ify, w których jedyną operacją w bloku jest deklaracja, są dozwolone, gdyż nie widzę powodu aby było inaczej. 
Taka deklaracja traktowana jest wówczas jako deklaracja wewnątrz bloku, a więc taka zmienna nie będzie widoczna nigdzie.
Wyjątkiem w tym wypadku jest `if(true) int a;` gdyż zostanie to uproszczone do `int a;` i wówczas `a` będzie widoczne 
poza ifem/pętlą.


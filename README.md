# Floyd

Description in english in progress.

Z małymi modyfikacjami udało mi się zaimplementować wszystkie cechy wymienione przy deklaracji.
Małe modyfikacje obejmują
- Nie ma konkatenacji stringów.
- Nie posiadam jednej funkcji `print` oraz `toString`. Zamiast tego, zaimplementowałem trzy metody: `printBool`, `printString`, `printInt`.
- Zaimplementowałem znak specjalny `_`, pomocny  przy przypisywaniu do krotek.
- Porównywanie dotyczy nie tylko zmiennych typu `int`. Porównywalny jest każdy typ z wyjątkiem typu funkcji. Nie da się porównać wartości różnych typów, taka próba skończy się błędem podczas statycznej analizy. Typ `tuple` porównywalny jest po współrzędnych, przykładowo `[1, ["a"]] < [2, ["b"]]`, ale `!([1, ["b"]] =< [2, ["a"]])` oraz `!([1, ["b"]] >= [2, ["a"]])`.
- Można mieć "zmienne" typu `void`. Nie ma dla nich jednak żadnego sensownego zastosowania. Są trochę jak typ `unit` w Haskellu.

Język jest statycznie typowany. Nie posiada domyślnych wartości - użycie jakiejkolwiek zmiennej zanim wartość została do niej przypisana kończy się wypisaniem kommunikatu o błędzie.

#### Funkcje wbudowane

1. `void assert(bool)` - kończy wykonanie programu gdy napotka na `false`.
2. `printInt, printString, printBool` - wszystkie metody zwracające `void`, przyjmują  (przez wartość) odpowiednio `int`, `string` oraz `bool`. Wypisują wartość argumentu na ekranie wraz ze znakiem nowej linii.

#### Funkcje wyższego rzędu, lambdy

W pliku `good/17.fl` został przedstawiony ich syntax. Wbrew deklaracji, nie jest tak, że przy przekazywaniu funkcji zawsze przyjmowana jest semantyka referencji. Jednak (co jest przedstawione na przykładach) przekazanie funkcji lambdy będaćej domknięciem i wywołanie jej w tejże funkcji, wpływa na jej następne wywołanie poza funkcją.
Lambdy można przekazywać przez referencję, funkcji zagnieżdżonych nie.
W przypadku posiadania zmiennej i funkcji o tej samej nazwie priorytet związany z identyfikatorem ma zmienna (`bad/functionAndVarOfSameName.fl`).

#### Tuple

Działanie przedstawione w pliku `good/15.fl`. 
Rozpakowując obiekt `tuple` można użyć specjalnego znaku `_` by zignorować daną pozycję. Dalej będzie ona jednak ewaluowana.

### Interpreter

**Uruchomienie**

- `make`- Kompiluje pliki *.hs i generuje binarkę interpretera `interpreter.
- `make clean` - Usuwa pliki powstałe podczas kompilacji. 

#### Biarka interpretera

- `./interpreter --help` - Wyświetla informację na temat sposobu używania binarki.
- `./interpreter [plik]` - Interpretuje program w pliku, który został podany jako argument.
- `./interpreter` - Interpretuje program podany który został podany na standardowe wejście.

Jeżeli uruchomienie programu skończy się błędem, binarka wypisuje komunikat błędu na *stderr*. Uruchomienie interpretera kończy się kodem, z jakim zakończył się program (w przypadku błędu będzie się kończył kodem `1`).

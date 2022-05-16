# Floyd

Z małymi modyfikacjami udało mi się zaimplementować wszystkie cechy wymienione przy deklaracji.
Małe modyfikacje obejmują
- Nie posiadam jednej funkcji `print` oraz `toString`. Zamiast tego, zaimplementowałem trzy metody: `printBool`, `printString`, `printInt`.
- Zaimplementowałem znak specjalny `_`, pomocny  przy przypisywaniu do krotek.
- Porównywanie dotyczy nie tylko zmiennych typu `int`. Porównywalny jest każdy typ z wyjątkiem typu funkcji. Nie da się porównać wartości różnych typów, taka próba skończy się błędem podczas statycznej analizy. Typ `tuple` porównywalny jest po współrzędnych, przykładowo `[1, ["a"]] < [2, ["b"]]`, ale `!([1, ["b"]] =< [2, ["a"]])` oraz `!([1, ["b"]] >= [2, ["a"]])`.
- Można mieć "zmienne" typu `void`. Nie ma dla nich jednak żadnego sensownego zastosowania. Są trochę jak typ `unit` w Haskellu.

Język jest statycznie typowany. Nie posiada domyślnych wartości - użycie jakiejkolwiek zmiennej zanim wartość została do niej przypisana kończy się wypisaniem kommunikatu o błędzie.


### Interpreter

**Uruchomienie**

- `make`- Kompiluje pliki *.hs i generuje binarkę interpretera `interpreter.
- `make clean` - Usuwa pliki powstałe podczas kompilacji. 

#### Biarka interpretera

- `./interpreter --help` - Wyświetla informację na temat sposobu używania binarki.
- `./interpreter [plik]` - Interpretuje program w pliku, który został podany jako argument.
- `./interpreter` - Interpretuje program podany który został podany na standardowe wejście.

Jeżeli uruchomienie programu skończy się błędem, binarka wypisuje komunikat błędu na *stderr*. Uruchomienie interpretera kończy się kodem, z jakim zakończył się program (w przypadku błędu będzie się kończył kodem `1`).

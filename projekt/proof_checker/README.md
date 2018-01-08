# Proof checker
#### Wojtek Fica

### Specyfikacja
Implementacja podstawowej wersji zadania z dodatkami:
- Aksjomaty
- Logika klasyczna (prawo eliminacji podwójnej negacji)
- Logika pierwszego rzędu
- Program próbuje uzupełnić luki w dowodzie


### Struktura projektu
Najważniejszy fragment zadania jest napisany w plikach
- [rules.ml](rules.ml)
- [natural_deduction.ml](natural_deduction.ml)
- [proof_assistant.ml](proof_assistant.ml)

### Kompilacja
- kompilacja: ```make proof_checker```
- sprzątanie: ```make clean```

### Uruchomienie
```./proof_checker.native in out```

### Składnia
Składnia jak w specyfikacji zadania, ale:
- nazwa zmiennej dopasować się do następującego wyrażenia regularnego: ```(['B'-'D'] | ['F' - 'Z']) ['a'-'z' 'A'-'Z' '0'-'9' '_']*```
- nazwa celu dopasować się do następującego wyrażenia regularnego: ```['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*```
- logika I rzędu: ```A x formula```, ```E x formula```
- luki w dowodzie: ```prove_it { formula }```
- w folderze ```tests/``` dostarczone są testy z przykładowymi dowodami

### Testy
| Test      | Opis                       | Rozszerzenie          |
| --------- | -------------------------- | --------------------- |
| tests/in0 | test z poprawnymi dowodami | wersja bez rozszerzeń |
| tests/in1 | test z błędnymi dowodami   | wersja bez rozszerzeń |
| tests/in2 | test z poprawnymi dowodami | aksjomaty             |
| tests/in3 | test z błędnymi dowodami   | logika I rzędu        |
| tests/in4 | test z poprawnymi dowodami | logika I rzędu        |
| tests/in5 | test z poprawnymi dowodami | ```prove_it {}```     |
| tests/in6 | test z błędnymi dowodami   | ```prove_it {}```     |


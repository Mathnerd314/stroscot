Stroscot supports your typical arithmetic operations:
```
pi_approx = 3+1/(7+1/(15+1/1))
# 355/113 = 3.14159292035...
```
For brevity, leading and trailing parentheses can be omitted:
```
pi_approx = 3+1/(7+1/(15+1/1
# 355/113
```
If you don't like this, you can set Stroscot to warn or error on unmatched parentheses.
```
stroscot -Wunmatched_parentheses pi_approx
# Warning: unmatched parentheses
# 355/113
stroscot -Eunmatched_parentheses pi_approx
# Error: unmatched parentheses
# Aborted.
```
A gentler approach is to reformat the code to add parentheses:
```
stroscot -Funmatched_parentheses pi_approx
# Log: fixing unmatched parentheses
```
This option is on by default and running a source code formatter is always good practice.

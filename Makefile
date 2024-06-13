# Makefile para compilar o código Haskell

# Compilador Haskell
GHC = ghc

# Opções de compilação
GHC_OPTS = -Wall

# Nome do executável
EXECUTABLE = tableaux

# Arquivo fonte
SOURCE = tableaux.hs

# Regra para construir o executável
$(EXECUTABLE): $(SOURCE)
    $(GHC) $(GHC_OPTS) -o $(EXECUTABLE) $(SOURCE)

# Regra para limpar arquivos temporários e o executável
clean:
    rm -f $(EXECUTABLE) *.o *.hi

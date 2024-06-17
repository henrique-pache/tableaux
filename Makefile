# Makefile para compilar e executar o programa Haskell

# Comando para compilar o programa Haskell
ghc_opts = -Wall -O2

# Nome do executável gerado
executable = logical_formulas

# Lista de fontes Haskell
sources = Main.hs

# Comando para construir o executável
build:
	ghc $(ghc_opts) -o $(executable) $(sources)

# Comando para limpar arquivos gerados pela compilação
clean:
	rm -f $(executable) *.o *.hi

# Comando para rodar o programa com os exemplos
run:
	./$(executable)

# Comando para compilar e rodar o programa
all: build run

# Regras phony para garantir que não haja conflito com arquivos de mesmo nome
.PHONY: build clean run all

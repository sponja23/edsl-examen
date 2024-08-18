# Examen de "Abordaje Funcional a EDSLs"

La resolución de los ejercicios está separada en módulos:
- **Ejercicio 1**: [`ShallowEmbedding`](./src/ShallowEmbedding.hs).
- **Ejercicio 2**: [`DeepEmbedding`](./src/DeepEmbedding.hs).
- **Ejercicio 3**:
  - **Parte a**: [`ShallowEmbedding`](./src/ShallowEmbedding.hs) y [`DeepEmbedding`](./src/DeepEmbedding.hs).
  - **Parte b**: [`Parsing.ExprParser`](./src/Parsing/ExprParser.hs)
  - **Parte c**: [`Parsing.Parser`](./src/Parsing/Parser.hs) y [`Parsing.ExprParser`](./src/Parsing/ExprParser.hs).
- **Ejercicio 4**:
  - **Parte a**: [`Extended.ShallowEmbedding`](./src/Extended/ShallowEmbedding.hs).
  - **Parte b**: [`Extended.DeepEmbedding`](./src/Extended/DeepEmbedding.hs).
  - **Parte c**: [`Extended.ExprParser`](./src/Extended/ExprParser.hs).
  - **Parte d**: [`Extended.ExprParser`](./src/Extended/ExprParser.hs).

Usé `cabal` para configurar el proyecto, e instalar dependencias para los tests. Éstos se pueden correr mediante:
```bash
cabal test
```

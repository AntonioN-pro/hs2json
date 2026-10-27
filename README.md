# hs2json — Biblioteca JSON, Pretty Printing e Testes com QuickCheck em Haskell

Projeto desenvolvido para a **Avaliação da Unidade 2** da disciplina de **Programação Funcional (2026.2)** na UFMA, guiado pelo tutorial oficial:
🔗 [Construindo e Testando uma Biblioteca Haskell: JSON, Pretty Printing e QuickCheck](https://lambdageo-edu.github.io/lambdageo-tutorials/haskell/)

---

## 👥 Integrantes da Dupla

- **Antonio Neto** — `antonio.melo@stc.ma.gov.br`
- **Gabriel Mesquita Torres**

---

## 🎥 Vídeo de Apresentação
> 🔗 **https://drive.google.com/file/d/1hnsVHVpHcFpMwzPi8GvoyOGjpQM70Xno/view?usp=sharing**

---

## 🏗️ Arquitetura do Projeto

O projeto é estruturado utilizando o **Cabal** (`cabal-version: 3.0`) e divide-se nos seguintes módulos:

```text
hs2json/
├── hs2json.cabal          # Especificação do pacote, biblioteca, executável e suíte de testes
├── src/
│   ├── SimpleJSON.hs      # Tipo algébrico JValue e funções seguras de extração (accessors)
│   ├── PutJSON.hs         # Renderizador JSON direto para String e impressão IO
│   ├── Prettify.hs        # Biblioteca de formatação baseada na árvore abstrata Doc
│   ├── PrettyJSON.hs      # Renderizador JSON sobre Doc com escapes Unicode e surrogate pairs
│   └── QuickTestes.hs     # Estudo preliminar de QuickCheck com qsort e propriedades de modelo
├── app/
│   └── Main.hs            # Executável demonstrando PutJSON, compact, pretty e accessors
└── test/
    └── Spec.hs            # Suíte QuickCheck (Arbitrary Doc, Arbitrary JValue e 19 propriedades)
```

### Descrição dos Módulos (`src/`)

1. **`SimpleJSON`**:
   - Define o tipo de dados algébrico `JValue` (`JString`, `JNumber`, `JBool`, `JNull`, `JObject`, `JArray`).
   - Implementa funções seguras de extração com `Maybe`: `getString`, `getInt`, `getDouble`, `getBool`, `getObject`, `getArray` e o predicado `isNull`.
2. **`PutJSON`**:
   - Primeira abordagem de renderização direta (`renderJValue :: JValue -> String`), separando código funcional puro da ação de saída (`putJValue :: JValue -> IO ()`).
3. **`Prettify`**:
   - Biblioteca genérica de *pretty printing* construída sobre o tipo algébrico `Doc` (`Empty`, `Char`, `Text`, `Line`, `Concat`, `Union`).
   - Implementa combinadores de documentos (`empty`, `char`, `text`, `double`, `line`, `(<>)`, `hcat`, `fsep`, `(</>)`, `punctuate`, `group`, `softline`) e dois motores de renderização:
     - `compact :: Doc -> String`: formatação compacta para transmissão máquina-a-máquina.
     - `pretty :: Int -> Doc -> String`: formatação adaptativa baseada na largura máxima de colunas (`fits` / `nicest`).
4. **`PrettyJSON`**:
   - Renderizador JSON construído sobre `Prettify.Doc`.
   - Realiza o escape completo de caracteres especiais (`simpleEscapes`), caracteres de controle/ASCII não imprimíveis (`smallHex` com `\uXXXX`) e caracteres Unicode acima de `0xFFFF` via pares substitutos (*surrogate pairs* na função `astral` com `shiftR` e `.&.`).
   - Usa o combinador de alta ordem `series` para formatar listas (`JArray`) e objetos (`JObject`).
5. **`QuickTestes`**:
   - Módulo introdutório a testes baseados em propriedades com `QuickCheck` sobre `qsort` (idempotência, ordenação, permutação, extremos com `==>` e comparação com o modelo `Data.List.sort`).

---

## ⚠️ Ponto Crítico: Gerador `Arbitrary` para `JValue` (`sized` + `resize`)

O tipo `JValue` possui recursão indireta através de listas em `JObject [(String, JValue)]` e `JArray [JValue]`.
Apenas reduzir o parâmetro `n` com `sized` nas chamadas recursivas **não é suficiente**: o combinador `listOf` consulta o tamanho **ambiente** do QuickCheck (que cresce até 100), fazendo com que cada nível da árvore possa ter dezenas de elementos e causando explosão exponencial de memória (travamento do processo).

Em `test/Spec.hs`, solucionamos esse problema envolvendo `listOf` com `resize (n `div` 2)`:

```haskell
instance Arbitrary JValue where
    arbitrary = sized genJValue

genJValue :: Int -> Gen JValue
genJValue n
  | n <= 0    = leaf
  | otherwise = oneof [leaf, recursive]
  where
    leaf = oneof
      [ JString <$> arbitrary
      , JNumber <$> arbitrary
      , JBool   <$> arbitrary
      , pure JNull
      ]
    recursive = oneof
      [ JObject <$> resize (n `div` 2) (listOf field)
      , JArray  <$> resize (n `div` 2) (listOf smaller)
      ]
    field   = (,) <$> arbitrary <*> smaller
    smaller = genJValue (n `div` 2)
```

Dessa forma, tanto a **profundidade** da árvore quanto a **largura** das listas diminuem geometricamente a cada nível.

---

## 🚀 Como Compilar, Executar e Testar

### Pré-requisitos
- **GHC** 9.x e **Cabal** 3.x (recomendado instalar via [GHCup](https://www.haskell.org/ghcup/)).

### 1. Compilar o projeto
```bash
cabal build
```

### 2. Executar o programa de demonstração
```bash
cabal run
```

### 3. Executar a suíte de testes (QuickCheck)
```bash
cabal test --test-show-details=direct
```

### 4. Gerar relatório de cobertura de testes (HPC)
```bash
cabal test --enable-coverage --test-show-details=direct
```

---

## 🧪 Propriedades Verificadas em `test/Spec.hs`

A suíte executa automaticamente todas as propriedades via Template Haskell (`$quickCheckAll`):

### Propriedades sobre `Doc` (`Prettify`)
- `prop_empty_id`: Identidade à esquerda e à direita de `empty` sob `(<>)`.
- `prop_char`, `prop_text`, `prop_line`, `prop_double`: Construtores básicos de `Doc`.
- `prop_hcat`: Equivalência da concatenação horizontal com fold explícito.
- `prop_punctuate'`: Modelo de intercalação de pontuação com fusão de `Empty`.
- `prop_compact_text` e `prop_pretty_text`: Preservação de strings em `compact` e `pretty`.

### Propriedades sobre `JValue` (`SimpleJSON` e `PrettyJSON`)
1. **Roundtrip dos Accessors** (`prop_getString_roundtrip`, `prop_getInt_truncate`, `prop_getDouble_roundtrip`, `prop_getBool_roundtrip`, `prop_getObject_roundtrip`, `prop_getArray_roundtrip`): Garante que cada accessor recupera exatamente o valor encapsulado pelo construtor correspondente.
2. **Invariante de Nulos e Segurança de Tipos** (`prop_isNull_correct`, `prop_accessors_safe_on_mismatch`): Verifica que `isNull` só é verdadeiro para `JNull` e que accessors aplicados a construtores distintos retornam `Nothing` com segurança.
3. **Corretude dos Escapes** (`prop_string_quotes_escaped`): Verifica que qualquer `JString` renderizada está entre aspas duplas e não contém aspas duplas ou caracteres de controle (`\n`, `\r`, `\t`) sem escape no seu interior.
4. **Idempotência e Totalidade de Renderização** (`prop_render_idempotent`): Garante que a renderização é total, nunca retorna string vazia e é idempotente quando re-empacotada como `Doc`.
5. **Delimitadores Estruturais** (`prop_containers_enclose`): Garante que todo `JObject` renderizado inicia com `{` e termina com `}`, e todo `JArray` inicia com `[` e termina com `]`.

---

## 📄 Licença

Distribuído sob a licença **BSD-3-Clause**. Veja o arquivo `LICENSE` para mais detalhes.

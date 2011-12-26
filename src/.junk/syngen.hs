-- Grammar
data Grammar = Grammar [NonTerminalBind] [VariableBind]

data NonTerminalBind = NonTerminalBind Name Rule

type Name = String

data Rule = Rule [Production]
          | Token Regexp

type Regexp = String

data Production = Production Constructor [Symbol]

type Constructor = String

data Symbol = Terminal String
            | NonTerminal Name Closure String
            | Variable Name

-- Inductive types

-- Representation of inductive type
#[water_drop]
pub fn exit(code: u32) extern

#[water_drop]
pub spec Copy

#[water_drop]
pub spec Drop {
	fn drop(self: ^mut Self)
}

#[water_drop]
pub enum [T] Option {
	None
	Some: T
}

impl [T: Copy] Copy for Option\[T]

#[water_drop]
pub enum TokenKind {
	Func
	Type
	Return
	Use
	Extern
	If
	Elif
	Else
	For
	Break
	Continue
	Let
	Struct
	Spec
	Enum
	Mut
	Impl
	As
	Match
	Pub
	Priv
	Const
	Comma
	Colon
	Dot
	RightArrow
	ThickRightArrow
	DoubleColon
	Hash
	DoubleHash
	BackSlash
	DoubleDot
	Tilde
	LeftCurly
	RightCurly
	LeftParen
	RightParen
	LeftBracket
	RightBracket
	Label
	Ident
	Int
	String
	Bool
	Char
	Comment
	Space
	Operator: u8
	NewLine
	Error
	Eof
	None
}

impl Copy for TokenKind

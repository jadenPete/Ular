use std::{
    fmt::{Display, Formatter},
    ops::Range,
};

const CONTEXT_LINES: usize = 2;

pub struct CompilationError {
    pub message: CompilationErrorMessage,
    pub position: Option<Position>,
}

pub enum CompilationErrorMessage {
    DuplicateField {
        field: String,
    },

    ExpectedFunctionResult {
        function_name: String,
        return_type: String,
    },
    
    ExpectedNumericType {
        actual_type: String,
    },

    IncorrectNumberOfArguments {
        expected: usize,
        actual: usize,
    },

    InternalError(InternalError),
    MissingFields {
        type_: String,
        fields: Vec<String>,
    },

    NestedFunctionsNotSupported,
    NumberOutOfRange {
        expected_type: Option<String>,
        value: i128,
        minimum: i128,
        maximum: i128,
    },

    StructAlreadyDefined {
        name: String,
    },

    StructFieldAlreadyDefined {
        field_name: String,
    },

    StructMethodAlreadyDefined {
        method_name: String,
    },

    StructEvadesScope {
        struct_name: String,
    },

    TypeMismatch {
        expected_type: String,
        actual_type: String,
    },

    UnknownField {
        type_: String,
        field: String,
    },

    UnknownMethod {
        type_: String,
        method: String,
    },

    UnknownType {
        name: String,
    },

    UnknownValue {
        name: String,
    },

    UnitPassedAsValue,
    ValueNotCallable,
    VariableAlreadyDefined {
        name: String,
    },
}

impl Display for CompilationErrorMessage {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::DuplicateField { field } => {
                write!(formatter, "The field `{}` was specified more than once.", field)
            }

            Self::ExpectedFunctionResult { function_name, return_type } => {
                write!(
                    formatter,
                    "Expected `{}` to return a value of type `{}`.",
                    function_name,
                    return_type,
                )
            }
            
            Self::ExpectedNumericType { actual_type } => {
                write!(
                    formatter,
                    "Expected a numeric value, but got one of type `{}`.",
                    actual_type,
                )
            }

            Self::IncorrectNumberOfArguments { expected, actual } => {
                let argument_word = if *expected == 1 {
                    "argument"
                } else {
                    "arguments"
                };

                write!(
                    formatter,
                    "Expected {} {}, but got {}.",
                    expected, argument_word, actual,
                )
            }

            Self::InternalError(error) => write!(formatter, "{}", error),
            Self::MissingFields { type_, fields } => {
                let field_word = if fields.len() == 1 { "field" } else { "fields" };

                write!(
                    formatter,
                    "Missing {} when constructing a value of type `{}`: ",
                    field_word,
                    type_,
                )?;

                let mut fields_iterator = fields.iter();

                if let Some(head) = fields_iterator.next() {
                    write!(formatter, "`{}`", head)?;

                    for field in fields_iterator {
                        write!(formatter, ", `{}`", field)?;
                    }
                }

                Ok(())
            }

            Self::NestedFunctionsNotSupported => {
                write!(formatter, "Nested functions aren't currently supported.")
            }

            Self::NumberOutOfRange {
                expected_type: Some(expected_type),
                value,
                minimum,
                maximum,
            } => {
                write!(
                    formatter,
                    "Expected a `{}`, but {} isn't between {} and {}.",
                    expected_type,
                    value,
                    minimum,
                    maximum,
                )
            }

            Self::NumberOutOfRange {
                expected_type: None,
                value,
                minimum,
                maximum,
            } => {
                write!(formatter, "The default numeric type is i32, but {} isn't between {} and {}. Consider using a different type.", value, minimum, maximum)
            }

            Self::StructAlreadyDefined { name } => {
                write!(formatter, "A struct with name `{}` was already defined.", name)
            }

            Self::StructEvadesScope { struct_name } => {
                write!(
                    formatter,
                    "A value of type `{}` evades the scope of `{}`.",
                    struct_name,
                    struct_name,
                )
            }

            Self::StructFieldAlreadyDefined { field_name } => {
                write!(formatter, "A field with name `{}` was already defined.", field_name)
            }

            Self::StructMethodAlreadyDefined { method_name } => {
                write!(formatter, "A method with name `{}` was already defined.", method_name)
            }

            Self::TypeMismatch {
                expected_type,
                actual_type,
            } => {
                write!(
                    formatter,
                    "Expected a value of type `{}`, but got one of type `{}`.",
                    expected_type,
                    actual_type,
                )
            }

            Self::UnknownField { type_, field } => {
                write!(formatter, "Type `{}` has no field named `{}`.", type_, field)
            }

            Self::UnknownMethod { type_, method } => {
                write!(formatter, "Type `{}` has no method named `{}`.", type_, method)
            }

            Self::UnknownType { name } => write!(formatter, "Unknown type `{}`.", name),
            Self::UnknownValue { name } => write!(formatter, "Unknown value `{}`.", name),
            Self::UnitPassedAsValue => write!(formatter, "It looks like you're trying to pass around `unit` as a value. This isn't currently supported."),
            Self::ValueNotCallable => {
                write!(formatter, "This value can't be called because it isn't a function.")
            }

            Self::VariableAlreadyDefined { name } => {
                write!(formatter, "A variable with name `{}` was already defined.", name)
            }
        }
    }
}

pub enum InternalError {
    AnalyzerStructNotDefined {
        index: usize,
    },

    AnalyzerFunctionNotDefined {
        index: usize,
    },

    JitCompilerExpectedNumericType {
        actual_type: String,
    },

    JitCompilerExpectedStructType {
        actual_type: String,
    },

    JitCompilerStructComparisonNotRewritten,
    JitCompilerTypeMismatch {
        expected_type: String,
        actual_value: String,
    },

    JitCompilerUnknownExpression {
        index: usize,
    },

    JitCompilerUnknownFunction {
        index: usize,
    },

    JitCompilerUnknownParameter {
        index: usize,
    },

    JitCompilerUnknownStructMethod {
        struct_index: usize,
        method_index: usize,
    },

    UnknownType {
        name: String,
    },

    UnknownValue {
        name: String,
    },

    VariableAlreadyDefined {
        name: String,
    },
}

impl Display for InternalError {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::AnalyzerStructNotDefined { index } => {
                write!(
                    formatter,
                    "The analyzer phase reserved struct {}, but didn't define it.",
                    index,
                )
            }

            Self::AnalyzerFunctionNotDefined { index } => {
                write!(
                    formatter,
                    "The analyzer phase reserved function {}, but didn't define it.",
                    index,
                )
            }

            Self::JitCompilerExpectedNumericType { actual_type } => {
                write!(
                    formatter,
                    "Expected a numeric value, but got one of type `{}`. This should've been caught by the typechecker.",
                    actual_type,
                )
            }

            Self::JitCompilerExpectedStructType { actual_type } => {
                write!(
                    formatter,
                    "Expected a struct value, but got one of type `{}`. This should've been caught by the typechecker.",
                    actual_type,
                )
            }

            Self::JitCompilerStructComparisonNotRewritten => {
                write!(
                    formatter,
                    "This struct comparison should've been rewritten in the typechecking phase, but it wasn't.",
                )
            }

            Self::JitCompilerTypeMismatch {
                expected_type,
                actual_value,
            } => {
                write!(
                    formatter,
                    "Attempted to coerce `{}` into a `{}`.",
                    actual_value, expected_type,
                )
            }

            Self::JitCompilerUnknownExpression { index } => {
                write!(
                    formatter,
                    "JIT compiler has no value for the expression with index {}.",
                    index,
                )
            }

            Self::JitCompilerUnknownFunction { index } => {
                write!(
                    formatter,
                    "JIT compiler has no value for the function with index {}.",
                    index,
                )
            }

            Self::JitCompilerUnknownParameter { index } => {
                write!(
                    formatter,
                    "JIT compiler has no value for the function parameter with index {}.",
                    index,
                )
            }

            Self::JitCompilerUnknownStructMethod { struct_index, method_index } => {
                write!(
                    formatter,
                    "JIT compiler has no value for struct method with struct index {} and method index {}.",
                    struct_index,
                    method_index,
                )
            }

            Self::UnknownType { name } => {
                write!(
                    formatter,
                    "Unknown type `{}`. This should've been caught by the typechecker.",
                    name,
                )
            }

            Self::UnknownValue { name } => {
                write!(
                    formatter,
                    "Unknown value `{}`. This should've been caught by the typechecker.",
                    name,
                )
            }

            Self::VariableAlreadyDefined { name } => {
                write!(
                    formatter,
                    "A variable with name `{}` was already defined. This should've been caught by the typechecker.",
                    name,
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Position(pub Range<usize>);

fn print_numbered_lines(numbered_lines: &[(usize, String)]) {
    if let Some((last_line_number, _)) = numbered_lines.last() {
        let maximum_line_length = format!("{}", last_line_number + 1).len();

        for (i, line) in numbered_lines {
            let mut line_lines = line.lines();
            let line_first_line = line_lines.next().unwrap_or("");

            println!(
                " {: >width$} │ {}",
                i + 1,
                line_first_line,
                width = maximum_line_length
            );

            for line_line in line_lines {
                println!(" {} │ {}", " ".repeat(maximum_line_length), line_line)
            }
        }
    }
}

pub fn report_error(error: CompilationError, source: &str) -> ! {
    match error.message {
        CompilationErrorMessage::InternalError(_) => {
            println!("Internal error: {}", error.message);
        }

        _ => {
            println!("Error: {}", error.message);
        }
    }

    if let Some(position) = error.position {
        let mut current_line_index = 0usize;
        let mut lines = Vec::<(usize, &str)>::new();

        for line in source.lines() {
            lines.push((current_line_index, line));

            current_line_index += line.len() + 1;
        }

        if lines.is_empty() {
            lines.push((0, ""));
        }

        // The zero-based index of the first line the error matches
        let start_line = lines
            .iter()
            .enumerate()
            .take_while(|(_, (line_index, _))| *line_index <= position.0.start)
            .last()
            .map(|(i, _)| i)
            .unwrap_or(0);

        // The zero-based index of the last line the error matches
        let end_line = lines[start_line..]
            .iter()
            .enumerate()
            .find(|(_, (line_index, _))| *line_index >= position.0.end)
            .map(|(i, _)| start_line + i - 1)
            .unwrap_or(lines.len() - 1);

        let start_line_with_context = start_line.saturating_sub(CONTEXT_LINES);
        let end_line_with_context = (end_line + CONTEXT_LINES).min(lines.len() - 1);
        let mut numbered_lines_with_marker = Vec::new();

        // In my opinion, this is clearer than:
        // ```
        // for (i, (line_index, line)) in lines[start_line_with_context..=end_line_with_context].iter().enumerate() {
        //     let j = i + start_line_with_context;
        //
        //     ...
        // }
        // ```
        #[allow(clippy::needless_range_loop)]
        for i in start_line_with_context..=end_line_with_context {
            let (line_index, line) = lines[i];

            if i != start_line {
                numbered_lines_with_marker.push((i, line.to_string()));
            } else {
                let start_column = position.0.start - line_index;
                let marker_length = if start_line == end_line {
                    position.0.end - position.0.start
                } else {
                    line.len() - start_column
                }
                .max(1);

                let line_with_marker = format!(
                    "{}\n{}{}",
                    line,
                    line[..start_column]
                        .chars()
                        .map(|character| if character == '\t' { '\t' } else { ' ' })
                        .collect::<String>(),
                    "^".repeat(marker_length)
                );

                numbered_lines_with_marker.push((i, line_with_marker));
            }
        }

        println!();

        print_numbered_lines(&numbered_lines_with_marker);

        println!();
    }

    std::process::exit(1)
}

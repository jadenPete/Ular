use crate::{arguments::Arguments, error_reporting::CompilationError};
use log::warn;
use std::fmt::Debug;

pub trait Phase<Input> {
    type Output: Debug;

    fn name() -> String;
    fn execute(&self, input: Input) -> Result<Self::Output, CompilationError>;

    fn execute_and_debug(
        &self,
        input: Input,
        arguments: &Arguments,
    ) -> Result<Self::Output, CompilationError> {
        let output = self.execute(input);
        let name = Self::name();

        if let Ok(ref output) = output {
            if arguments
                .debug_phase
                .iter()
                .any(|other_name| other_name.as_ref() == name)
            {
                warn!("Output of the {} phase:\n{:#?}", name, output);
            }
        }

        output
    }
}

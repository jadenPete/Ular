use log::debug;
use std::fmt::Debug;

pub trait Phase<Input, Output: Debug, Error> {
    fn name() -> String;
    fn execute(&self, input: Input) -> Result<Output, Error>;

    fn execute_and_debug<A: AsRef<str>>(
        &self,
        input: Input,
        debug_phases: &[A],
    ) -> Result<Output, Error> {
        let output = self.execute(input);
        let name = Self::name();

        match output {
            Ok(ref output) => {
                if debug_phases
                    .iter()
                    .any(|other_name| other_name.as_ref() == name)
                {
                    debug!("Output of the {} phase:", name);
                    debug!("{:?}", output);
                }
            }

            Err(_) => {}
        }

        output
    }
}

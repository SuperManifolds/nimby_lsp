use nimbyscript_parser::ast::Span;

/// Diagnostic severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// A diagnostic message
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub code: Option<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            code: None,
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
            code: None,
        }
    }

    pub fn info(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Info,
            message: message.into(),
            span,
            code: None,
        }
    }

    pub fn hint(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Hint,
            message: message.into(),
            span,
            code: None,
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    #[test]
    fn test_error_creation() {
        let d = Diagnostic::error("something went wrong", span(10, 20));
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.message, "something went wrong");
        assert_eq!(d.span.start, 10);
        assert_eq!(d.span.end, 20);
        assert!(d.code.is_none());
    }

    #[test]
    fn test_warning_creation() {
        let d = Diagnostic::warning("this is suspicious", span(0, 5));
        assert_eq!(d.severity, Severity::Warning);
        assert_eq!(d.message, "this is suspicious");
    }

    #[test]
    fn test_info_creation() {
        let d = Diagnostic::info("FYI", span(0, 1));
        assert_eq!(d.severity, Severity::Info);
        assert_eq!(d.message, "FYI");
    }

    #[test]
    fn test_hint_creation() {
        let d = Diagnostic::hint("consider this", span(5, 10));
        assert_eq!(d.severity, Severity::Hint);
        assert_eq!(d.message, "consider this");
    }

    #[test]
    fn test_with_code() {
        let d = Diagnostic::error("error", span(0, 1)).with_code("E0001");
        assert_eq!(d.code, Some("E0001".to_string()));
    }

    #[test]
    fn test_with_code_chain() {
        let d = Diagnostic::warning("warn", span(0, 10))
            .with_code("W0100");
        assert_eq!(d.severity, Severity::Warning);
        assert_eq!(d.code, Some("W0100".to_string()));
    }

    #[test]
    fn test_severity_equality() {
        assert_eq!(Severity::Error, Severity::Error);
        assert_ne!(Severity::Error, Severity::Warning);
        assert_ne!(Severity::Warning, Severity::Info);
        assert_ne!(Severity::Info, Severity::Hint);
    }

    #[test]
    fn test_diagnostic_clone() {
        let d1 = Diagnostic::error("test", span(0, 5)).with_code("E001");
        let d2 = d1.clone();
        assert_eq!(d1.message, d2.message);
        assert_eq!(d1.severity, d2.severity);
        assert_eq!(d1.code, d2.code);
    }

    #[test]
    fn test_message_from_string() {
        let msg = String::from("dynamic message");
        let d = Diagnostic::error(msg, span(0, 1));
        assert_eq!(d.message, "dynamic message");
    }
}

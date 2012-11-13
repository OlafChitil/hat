-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment
  ) where


newtype Environment = Env Int
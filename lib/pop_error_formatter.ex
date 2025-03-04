defmodule PopErrorFormatter do
  @moduledoc """
  Formats error messages with colors and helpful suggestions, similar to Rust's error messages.
  """

  # ANSI color codes
  @red IO.ANSI.red()
  @green IO.ANSI.green()
  @yellow IO.ANSI.yellow()
  @blue IO.ANSI.blue()
  @reset IO.ANSI.reset()
  @bold IO.ANSI.bright()

  @doc """
  Format a lexer error with colors and suggestions.
  """
  def format_lexer_error({:error, {line, :pop_lexer, {:illegal, token}}, _}) do
    """
    #{@red}#{@bold}Error:#{@reset} Illegal character in source
      #{@blue}-->#{@reset} line #{line}
    #{@red}#{@bold}  |
    #{line} |#{@reset} ... #{@red}#{token}#{@reset} ...
    #{@red}#{@bold}  |     ^#{@reset}
    #{@yellow}#{@bold}Help:#{@reset} Unexpected character '#{token}'.
    #{suggestion_for_illegal_char(token)}
    """
  end

  @doc """
  Format a parser error with colors and suggestions.
  """
  def format_parser_error({:error, {line, :pop_parser, [message, token]}}) do
    """
    #{@red}#{@bold}Error:#{@reset} #{message}
      #{@blue}-->#{@reset} line #{line}
    #{@red}#{@bold}  |
    #{line} |#{@reset} ... #{@red}#{token}#{@reset} ...
    #{@red}#{@bold}  |     ^#{@reset}
    #{@yellow}#{@bold}Help:#{@reset} #{suggestion_for_parser_error(message, token)}
    """
  end

  @doc """
  Format a compiler error with colors and suggestions.
  """
  def format_compiler_error({:error, errors, _}) do
    errors_str = Enum.map(errors, fn {file, file_errors} ->
      Enum.map(file_errors, fn {line, _module, error} ->
        """
        #{@red}#{@bold}Error:#{@reset} #{format_compiler_error_message(error)}
          #{@blue}-->#{@reset} #{file}:#{line}
        #{@red}#{@bold}  |
        #{line} |#{@reset} ...
        #{@red}#{@bold}  |     ^#{@reset}
        #{@yellow}#{@bold}Help:#{@reset} #{suggestion_for_compiler_error(error)}
        """
      end)
      |> Enum.join("\n")
    end)
    |> Enum.join("\n")

    """
    #{errors_str}
    #{@red}#{@bold}Compilation failed due to #{length(errors)} error(s).#{@reset}
    """
  end

  @doc """
  Format a runtime error with colors.
  """
  def format_runtime_error(error) do
    """
    #{@red}#{@bold}Runtime Error:#{@reset} #{inspect(error)}
    #{@yellow}#{@bold}Help:#{@reset} Check your program logic and ensure all variables are properly initialized.
    """
  end

  @doc """
  Format a general error message.
  """
  def format_error(message) do
    """
    #{@red}#{@bold}Error:#{@reset} #{message}
    """
  end

  @doc """
  Format a success message.
  """
  def format_success(message) do
    """
    #{@green}#{@bold}Success:#{@reset} #{message}
    """
  end

  # Private helper functions for generating suggestions

  defp suggestion_for_illegal_char(char) do
    case char do
      "!" -> "Did you mean '!=' for not equal comparison?"
      "&" -> "Did you mean '&&' for logical AND? Pop doesn't support this yet."
      "|" -> "Did you mean '||' for logical OR? Pop doesn't support this yet."
      ":" -> "Pop uses ';' for statement termination, not ':'."
      _ -> "This character is not recognized in Pop language."
    end
  end

  defp suggestion_for_parser_error(message, token) do
    cond do
      String.contains?(message, "syntax error") ->
        "Unexpected token '#{token}'. Check for missing semicolons or parentheses."

      String.contains?(message, "missing") && String.contains?(message, "semicolon") ->
        "Add a semicolon ';' at the end of the statement."

      String.contains?(message, "missing") && String.contains?(message, "parenthesis") ->
        "Add a closing parenthesis ')'."

      String.contains?(message, "missing") && String.contains?(message, "brace") ->
        "Add a closing brace '}'."

      true ->
        "Check your syntax around this token."
    end
  end

  defp format_compiler_error_message({:undefined_variable, var}) do
    "Undefined variable: '#{var}'"
  end

  defp format_compiler_error_message({:undefined_function, func, arity}) do
    "Undefined function: '#{func}/#{arity}'"
  end

  defp format_compiler_error_message(other) do
    inspect(other)
  end

  defp suggestion_for_compiler_error({:undefined_variable, var}) do
    "Make sure '#{var}' is defined before use. Check for typos in variable names."
  end

  defp suggestion_for_compiler_error({:undefined_function, func, _arity}) do
    "Function '#{func}' is not defined. Check for typos or define this function."
  end

  defp suggestion_for_compiler_error(_) do
    "Check your code for logical errors or type mismatches."
  end
end

defmodule PopErrorReporter do
  @moduledoc """
  Enhances error reporting in the Pop compiler by providing more context for errors.
  This module is used to intercept and enhance error messages from the Erlang compiler.
  """

  @doc """
  Extract source code context around an error.
  """
  def extract_source_context(filename, line_number, context_lines \\ 2) do
    try do
      case File.read(filename) do
        {:ok, content} ->
          lines = String.split(content, ~r/\r?\n/)

          # Calculate the range of lines to show
          start_line = max(1, line_number - context_lines)
          end_line = min(length(lines), line_number + context_lines)

          # Extract the relevant lines with line numbers
          Enum.map(start_line..end_line, fn i ->
            line = Enum.at(lines, i - 1)
            {i, line, i == line_number}
          end)

        {:error, _} ->
          []
      end
    rescue
      _ -> []
    end
  end

  @doc """
  Format source code context with line numbers and error indicators.
  """
  def format_source_context(context, error_column \\ nil) do
    Enum.map(context, fn {line_num, line, is_error_line} ->
      line_prefix = String.pad_leading("#{line_num} |", 6)

      if is_error_line do
        error_indicator = if error_column do
          padding = String.duplicate(" ", error_column + 6)
          "#{padding}^ Error occurs here"
        else
          "      ^ Error occurs here"
        end

        "#{IO.ANSI.red()}#{line_prefix}#{IO.ANSI.reset()} #{line}\n#{IO.ANSI.red()}#{error_indicator}#{IO.ANSI.reset()}"
      else
        "#{IO.ANSI.blue()}#{line_prefix}#{IO.ANSI.reset()} #{line}"
      end
    end)
    |> Enum.join("\n")
  end

  @doc """
  Suggest fixes for common errors.
  """
  def suggest_fix(error_type, details) do
    case error_type do
      :syntax_error ->
        "Check for missing semicolons, parentheses, or braces."

      :illegal_char ->
        char = to_string(details)
        case char do
          "@" -> "The '@' character is not valid in Pop. Did you mean to use an operator like '*' or '+'?"
          "&" -> "The '&' character is not valid in Pop. Did you mean '&&' for logical AND? Pop doesn't support this yet."
          "|" -> "The '|' character is not valid in Pop. Did you mean '||' for logical OR? Pop doesn't support this yet."
          ":" -> "Pop uses ';' for statement termination, not ':'."
          _ -> "The character '#{char}' is not recognized in Pop language."
        end

      :undefined_variable ->
        var_name = details
        "Variable '#{var_name}' is not defined. Make sure to initialize it before use."

      :undefined_function ->
        {func_name, arity} = details
        "Function '#{func_name}/#{arity}' is not defined. Check for typos or define this function."

      :type_error ->
        "Type mismatch. Ensure you're using compatible types in your operations."

      _ ->
        "Review your code for logical errors or syntax issues."
    end
  end

  @doc """
  Enhance a compiler error message with context and suggestions.
  """
  def enhance_error(error, filename) do
    case error do
      {:error, {line, :pop_lexer, {:illegal, token}}, _} ->
        context = extract_source_context(filename, line)
        formatted_context = format_source_context(context)

        """
        #{IO.ANSI.red()}#{IO.ANSI.bright()}Lexer Error:#{IO.ANSI.reset()} Illegal character in source
          #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

        #{formatted_context}

        #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:illegal_char, token)}
        """

      {:error, {line, :pop_parser, details}} ->
        context = extract_source_context(filename, line)
        formatted_context = format_source_context(context)

        """
        #{IO.ANSI.red()}#{IO.ANSI.bright()}Parser Error:#{IO.ANSI.reset()} Syntax error
          #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

        #{formatted_context}

        #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:syntax_error, details)}
        """

      {:error, errors, _} when is_list(errors) ->
        Enum.map(errors, fn {_file, file_errors} ->
          Enum.map(file_errors, fn {line, _module, details} ->
            context = extract_source_context(filename, line)
            formatted_context = format_source_context(context)

            error_type = case details do
              {:undefined_variable, _} -> :undefined_variable
              {:undefined_function, _, _} -> :undefined_function
              _ -> :unknown
            end

            """
            #{IO.ANSI.red()}#{IO.ANSI.bright()}Compiler Error:#{IO.ANSI.reset()} #{inspect(details)}
              #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

            #{formatted_context}

            #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(error_type, details)}
            """
          end)
          |> Enum.join("\n")
        end)
        |> Enum.join("\n")

      _ ->
        "#{IO.ANSI.red()}#{IO.ANSI.bright()}Error:#{IO.ANSI.reset()} #{inspect(error)}"
    end
  end
end

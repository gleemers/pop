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

      :unbound_var ->
        var_name = details
        "Variable '#{var_name}' is not defined. Make sure to initialize it before use."

      :undefined_function ->
        {func_name, arity} = details
        "Function '#{func_name}/#{arity}' is not defined. Check for typos or define this function."

      :type_error ->
        "Type mismatch. Ensure you're using compatible types in your operations."

      :constant_condition ->
        {always_value, expr} = details
        "This condition (#{inspect(expr)}) will #{if always_value, do: "always", else: "never"} be true."

      :unused_variable ->
        var_name = details
        "Variable '#{var_name}' is defined but never used."

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

      {:error, errors, warnings} when is_list(errors) ->
        error_messages = format_errors(errors, filename)
        warning_messages = format_warnings(warnings, filename)

        """
        #{error_messages}
        #{warning_messages}
        """

      _ ->
        "#{IO.ANSI.red()}#{IO.ANSI.bright()}Error:#{IO.ANSI.reset()} #{inspect(error)}"
    end
  end

  # Format a list of errors
  defp format_errors(errors, filename) do
    Enum.map(errors, fn
      {:error, {:undefined_variable, var}} ->
        # Find the line where the variable is used
        line = find_line_for_variable(filename, var)
        context = extract_source_context(filename, line)
        formatted_context = format_source_context(context)

        """
        #{IO.ANSI.red()}#{IO.ANSI.bright()}Compiler Error:#{IO.ANSI.reset()} #{inspect({:undefined_variable, var})}
          #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

        #{formatted_context}

        #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:undefined_variable, var)}
        """

      {:error, {:unbound_var, var}} ->
        # Find the line where the variable is used
        line = find_line_for_variable(filename, var)
        context = extract_source_context(filename, line)
        formatted_context = format_source_context(context)

        """
        #{IO.ANSI.red()}#{IO.ANSI.bright()}Compiler Error:#{IO.ANSI.reset()} #{inspect({:unbound_var, var})}
          #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

        #{formatted_context}

        #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:unbound_var, var)}
        """

      {:error, {:undefined_function, func, arity}} ->
        # Find the line where the function is called
        line = find_line_for_function_call(filename, func)
        context = extract_source_context(filename, line)
        formatted_context = format_source_context(context)

        """
        #{IO.ANSI.red()}#{IO.ANSI.bright()}Compiler Error:#{IO.ANSI.reset()} #{inspect({:undefined_function, func, arity})}
          #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

        #{formatted_context}

        #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:undefined_function, {func, arity})}
        """

      other ->
        """
        #{IO.ANSI.red()}#{IO.ANSI.bright()}Compiler Error:#{IO.ANSI.reset()} #{inspect(other)}
          #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:1

        #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} Check your code for errors.
        """
    end)
    |> Enum.join("\n")
  end

  @doc """
  Format warnings for display.
  """
  def format_warnings(warnings, filename) do
    do_format_warnings(warnings, filename)
  end

  # Format a list of warnings
  defp do_format_warnings(warnings, filename) do
    if Enum.empty?(warnings) do
      ""
    else
      warnings_str = Enum.map(warnings, fn
        {:warning, {:constant_condition, always_value, expr}} ->
          # Find the line where the condition appears
          line = find_line_for_condition(filename, expr)
          context = extract_source_context(filename, line)
          formatted_context = format_source_context(context)

          """
          #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Warning:#{IO.ANSI.reset()} Constant condition
            #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

          #{formatted_context}

          #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:constant_condition, {always_value, expr})}
          """

        {:warning, {:unused_variable, var}} ->
          # Find the line where the variable is defined
          line = find_line_for_variable_definition(filename, var)
          context = extract_source_context(filename, line)
          formatted_context = format_source_context(context)

          """
          #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Warning:#{IO.ANSI.reset()} Unused variable
            #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:#{line}

          #{formatted_context}

          #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} #{suggest_fix(:unused_variable, var)}
          """

        other ->
          """
          #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Warning:#{IO.ANSI.reset()} #{inspect(other)}
            #{IO.ANSI.blue()}-->#{IO.ANSI.reset()} #{filename}:1

          #{IO.ANSI.yellow()}#{IO.ANSI.bright()}Help:#{IO.ANSI.reset()} Review your code.
          """
      end)
      |> Enum.join("\n")

      """
      #{warnings_str}
      """
    end
  end

  # Find the line where a variable is used
  defp find_line_for_variable(filename, var) do
    try do
      case File.read(filename) do
        {:ok, content} ->
          lines = String.split(content, ~r/\r?\n/)

          # Look for the variable in each line
          Enum.find_index(lines, fn line ->
            # This is a simple heuristic - in a real implementation, you'd use the AST
            # with source location information
            String.contains?(line, to_string(var)) and
            (String.contains?(line, "#{var};") or
             String.contains?(line, "#{var} ") or
             String.contains?(line, "#{var})") or
             String.contains?(line, "#{var},") or
             String.match?(line, ~r/return\s+#{var}/))
          end) || 0
          |> Kernel.+(1)  # Convert to 1-indexed

        {:error, _} ->
          1
      end
    rescue
      _ -> 1
    end
  end

  # Find the line where a variable is defined
  defp find_line_for_variable_definition(filename, var) do
    try do
      case File.read(filename) do
        {:ok, content} ->
          lines = String.split(content, ~r/\r?\n/)

          # Look for variable definition in each line
          Enum.find_index(lines, fn line ->
            # This is a simple heuristic - in a real implementation, you'd use the AST
            # with source location information
            String.contains?(line, "#{var} =")
          end) || 0
          |> Kernel.+(1)  # Convert to 1-indexed

        {:error, _} ->
          1
      end
    rescue
      _ -> 1
    end
  end

  # Find the line where a function is called
  defp find_line_for_function_call(filename, func) do
    try do
      case File.read(filename) do
        {:ok, content} ->
          lines = String.split(content, ~r/\r?\n/)

          # Look for function call in each line
          Enum.find_index(lines, fn line ->
            # This is a simple heuristic - in a real implementation, you'd use the AST
            # with source location information
            String.contains?(line, "#{func}(")
          end) || 0
          |> Kernel.+(1)  # Convert to 1-indexed

        {:error, _} ->
          1
      end
    rescue
      _ -> 1
    end
  end

  # Find the line where a condition appears
  defp find_line_for_condition(filename, expr) do
    try do
      case File.read(filename) do
        {:ok, content} ->
          lines = String.split(content, ~r/\r?\n/)

          # Look for the condition in each line
          # This is a very simple heuristic that won't work for all cases
          expr_str = inspect(expr)
          |> String.replace("{op, ", "")
          |> String.replace("}", "")

          Enum.find_index(lines, fn line ->
            String.contains?(line, "if") and
            (String.contains?(line, "==") or
             String.contains?(line, "!=") or
             String.contains?(line, ">") or
             String.contains?(line, "<"))
          end) || 0
          |> Kernel.+(1)  # Convert to 1-indexed

        {:error, _} ->
          1
      end
    rescue
      _ -> 1
    end
  end
end

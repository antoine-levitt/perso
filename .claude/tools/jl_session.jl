# jl_session.jl — loaded into the warm REPL by jlrepl.sh.
#
# jlx(infile, outfile): run the code in `infile`, capturing stdout+stderr (and any
# error) to `outfile`, then create `outfile*".done"` so the client reads the result
# from a file instead of scraping the terminal. Tracks nothing on its own —
# includet the repo's source in your first eval so edits hot-reload.

using Revise

function jlx(infile::AbstractString, outfile::AbstractString)
    done = outfile * ".done"
    try
        open(outfile, "w") do io
            try
                redirect_stdout(io) do
                    redirect_stderr(io) do
                        Revise.revise()        # apply edits to tracked files
                        code = read(infile, String)
                        # Anchor at the project dir so a relative include() in `code`
                        # resolves against pwd(), not the temp dir.
                        Base.include_string(Main, code, joinpath(pwd(), "jlrepl_eval.jl"))
                    end
                end
            catch err
                print(io, "\n[error] ")
                showerror(io, err, catch_backtrace())
                println(io)
            end
        end
    finally
        touch(done)   # always signal completion, so the client never hangs
    end
    nothing
end

haskey(ENV, "JLREPL_READY") && touch(ENV["JLREPL_READY"])
@info "jl_session ready"

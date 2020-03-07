module SymbolServer

conn = stdout
(outRead, outWrite) = redirect_stdout()

using Serialization, Pkg, SHA
include("from_static_lint.jl")

server = Server(abspath(joinpath(@__DIR__, "..", "..", "store")), Pkg.Types.Context(), Dict{String,Any}())
if Sys.isunix()
    global const nullfile = "/dev/null"
elseif Sys.iswindows()
    global const nullfile = "nul"
else
    error("Platform not supported")
end

while true
    message, payload = deserialize(stdin)
    if message == :debugmessage
        @info(payload)
        serialize(conn, (:success, nothing))
    elseif message == :close
        break
    elseif message == :get_context
        serialize(conn, (:success, server.context))
    elseif message == :load_core
        core_pkgs = load_core()
        SymbolServer.save_store_to_disc(core_pkgs["Base"], joinpath(server.storedir, "Base.jstore"))
        SymbolServer.save_store_to_disc(core_pkgs["Core"], joinpath(server.storedir, "Core.jstore"))
        serialize(conn, (:success, nothing))
    elseif message == :load_package
        pkg = PackageID(first(payload), last(payload))
        SymbolServer.import_package_names(pkg, server.depot, server.context)
        for (uuid, pkg) in server.depot
            SymbolServer.save_store_to_disc(pkg, joinpath(server.storedir, "$uuid.jstore"))
        end
        serialize(conn, (:success, [k=>v.name for (k,v) in server.depot if v.name isa String]))
    elseif message == :load_all
        for pkg in SymbolServer.context_deps(server.context)
            SymbolServer.import_package_names(PackageID(first(pkg), string(last(pkg))), server.depot, server.context)
        end
        
        for (uuid, pkg) in server.depot
            SymbolServer.save_store_to_disc(pkg, joinpath(server.storedir, "$uuid.jstore"))
        end
        serialize(conn, (:success, [k=>v.name for (k,v) in server.depot if v.name isa String]))
    else
        serialize(conn, (:failure, nothing))
    end
end

end

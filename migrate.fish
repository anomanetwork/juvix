#!/usr/bin/env fish

cd ./tests/Casm
for dir in (fd -a -t f "Package.juvix" | xargs -I{} dirname "{}")
    set witness "$dir/.migrated"
    if test -e $witness
        echo "Already processed: $dir"
    else
        echo "Processing $dir"
        if juvix format --migration export-constructors $dir
            echo "Success"
            touch $witness
        end
    end
end

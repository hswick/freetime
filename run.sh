elm make src/Main.elm --optimize --output=freetime_app/assets/index.html
cd freetime_app
cargo run --release
cd ..

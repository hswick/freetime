#![windows_subsystem = "windows"]

extern crate web_view;
#[macro_use] extern crate nickel;
extern crate serde_json;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate nickel_sled;

use web_view::*;
use std::thread;
use std::str;

use nickel::{Nickel, HttpRouter, StaticFilesHandler, JsonBody};
use nickel::status::StatusCode;

use nickel_sled::{SledMiddleware, SledRequestExtensions};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct HourUnit {
    date_hour: String,
    content: String
}

fn run_server() {

    thread::spawn(|| {
        
        let mut server = Nickel::new();
        
        let s = SledMiddleware::new();
        server.utilize(s);

        let index_file: String = std::fs::read_to_string("assets/index.html").expect("Could not find index.html in assets");

        server.get("/", middleware!(&index_file[..]));
        server.utilize(StaticFilesHandler::new("assets/"));
        
        server.post("/week", middleware! { |request, response|                              
            
            let week_request = try_with!(response, {
                request.json_as::<Vec<String>>().map_err(|e| (StatusCode::BadRequest, e))
            });
            
            let mut  week: Vec<HourUnit> = Vec::<HourUnit>::new();
            
            for date in week_request.iter() {
                
                for i in 8..21 {

                    let date_hour = format!("{}_{:?}", &date, i);                    
                    
                    let content = match request.sled_conn().get(&date_hour.as_bytes()) {
                        Ok(Some(value)) => str::from_utf8(&value).unwrap().to_string(),
                        Ok(None) => "".to_string(),
                        Err(err) => err.to_string()
                     };                     

                     week.push(HourUnit { date_hour: date_hour, content: content });
                }
                
            } 
            
            let w = match serde_json::to_string(&week) {
                Ok(json) => json,
                Err(err) => err.to_string()
            };

            println!("{}", w);

            w
        });
        
        server.post("/hour", middleware! { |request, response|

            let hour_unit = try_with!(response, {
                request.json_as::<HourUnit>().map_err(|e| (StatusCode::BadRequest, e))
            });
            
            request.sled_conn().set(
                hour_unit.date_hour.as_bytes(), 
                hour_unit.content.as_bytes().to_vec()
            ).unwrap();

            let hu = match serde_json::to_string(&hour_unit) {
                Ok(json) => json,
                Err(err) => err.to_string()
            };

            println!("{}", hu);
                                            
            "Timeslot updated"
        });
        
        server.listen("127.0.0.1:6767").unwrap();
    });
}

fn display() {
    web_view::builder()
        .title("free your time, free you mind")
        .content(Content::Url("http://localhost:6767/"))
        .size(800, 600)
        .resizable(true)
        .debug(true)
        .user_data(())
        .invoke_handler(|_webview, _arg| Ok(()))
        .run()
        .unwrap();
}

fn main() {
    run_server();
    display();
}

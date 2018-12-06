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
    content: Option<String>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DayResponse {
    hour_units: Vec<HourUnit>
}

fn run_server() {

    //thread::spawn(|| {
        
        let mut server = Nickel::new();
        
        let s = SledMiddleware::new();
        server.utilize(s);
        
        server.get("/", middleware!("<div id='app'> Hello, World! </div>"));
        server.utilize(StaticFilesHandler::new("assets/"));
        
        server.get("/day/:date", middleware! { |req|
            let date = req.param("date").unwrap();
            
            let mut units: Vec<HourUnit> = Vec::<HourUnit>::new();

            for _ in 8..20 {
                let content = match req.sled_conn().get(&date) {
                    Ok(Some(value)) => Some(str::from_utf8(&value).unwrap().to_string()),
                    Ok(None) => None,
                    Err(err) => Some(err.to_string())
                };

                units.push(HourUnit { date_hour : date.to_string().clone(), content: content })
            }
            
            let day_response = DayResponse { hour_units: units.clone() };
            
            match serde_json::to_string(&day_response) {
                Ok(json) => json,
                Err(err) => err.to_string()
            }
        });
        
        server.post("/hour", middleware! { |request, response|

            let hour_unit = try_with!(response, {
                request.json_as::<HourUnit>().map_err(|e| (StatusCode::BadRequest, e))
            });
            
            request.sled_conn().set(
                hour_unit.date_hour.as_bytes(), 
                hour_unit.content.unwrap().as_bytes().to_vec()
            ).unwrap();
            
            "Timeslot updated"
        });
        
        server.listen("127.0.0.1:6767").unwrap();
    //});
}

fn display() {
    web_view::builder()
        .title("free your time, free you mind")
        .content(Content::Url("http://localhost:6767"))
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
    //display();
}
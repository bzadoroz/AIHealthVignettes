# app.R
library(shiny)
library(bslib)
library(htmltools)
library(httr)
library(jsonlite)

# -----------------------------
# Supabase configuration
# -----------------------------

config <- fromJSON("config.json")

# Resolve envs into R variables (single source of truth)
SUPABASE_URL <- config$supabase_url
SUPABASE_KEY <- config$supabase_key
SUPABASE_TABLE <- config$supabase_table
APP_EMAIL <- config$app_email
APP_PASSWORD <- config$app_password

# Sign in with password grant (server-side)
supabase_sign_in <- function(email, password) {
  url <- paste0(SUPABASE_URL, "/auth/v1/token?grant_type=password")
  res <- POST(
    url,
    add_headers(`apikey` = SUPABASE_KEY, `Content-Type` = "application/json"),
    body = toJSON(list(email = email, password = password), auto_unbox = TRUE)
  )
  status <- status_code(res)
  txt <- content(res, as = "text", encoding = "UTF-8")
  if (status >= 200 && status < 300) {
    fromJSON(txt)
  } else {
    stop(sprintf("Supabase sign-in failed (%s): %s", status, txt))
  }
}

# Refresh token
supabase_refresh_token <- function(refresh_token) {
  url <- paste0(SUPABASE_URL, "/auth/v1/token?grant_type=refresh_token")
  res <- POST(
    url,
    add_headers(`apikey` = SUPABASE_KEY, `Content-Type` = "application/json"),
    body = toJSON(list(refresh_token = refresh_token), auto_unbox = TRUE)
  )
  status <- status_code(res)
  txt <- content(res, as = "text", encoding = "UTF-8")
  if (status >= 200 && status < 300) {
    out <- fromJSON(txt)
    list(access_token = out$access_token, expires_in = out$expires_in)
  } else {
    stop(sprintf("Supabase token refresh failed (%s): %s", status, txt))
  }
}

# Save to Supabase with Authorization: Bearer <token>


save_to_supabase_auth <- function(row, access_token, return_representation = TRUE) {
  stopifnot(nzchar(access_token))
  url <- paste0(SUPABASE_URL, "/rest/v1/", SUPABASE_TABLE)
  
  # UTC formatting for POSIXct columns - FIXED VERSION
  for (i in seq_along(row)) {
    if (inherits(row[[i]], "POSIXct")) {
      row[[i]] <- strftime(as.POSIXct(row[[i]], tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  }
  
  prefer_val <- if (isTRUE(return_representation)) "return=representation" else "return=minimal"
  
  headers <- add_headers(
    `apikey`          = SUPABASE_KEY,
    `Authorization`   = paste("Bearer", access_token),
    `Content-Type`    = "application/json",
    `Accept`          = "application/json",
    `Prefer`          = prefer_val,
    `Content-Profile` = "public",
    `Accept-Profile`  = "public"
  )
  
  # Debug what's actually being sent
  json_body <- toJSON(row, auto_unbox = TRUE, na = "null")
  cat("DEBUG: Final JSON body:", json_body, "\n")
  
  res <- POST(url, headers, body = json_body)
  status <- status_code(res)
  txt <- content(res, as = "text", encoding = "UTF-8")
  
  cat("DEBUG: Supabase response status:", status, "\n")
  cat("DEBUG: Supabase response:", txt, "\n")
  
  if (status >= 200 && status < 300) {
    TRUE
  } else {
    stop(sprintf("Supabase insert failed (%s): %s", status, txt))
  }
}

supabase_pid_exists <- function(pid, access_token = NULL) {
  stopifnot(nzchar(SUPABASE_URL), nzchar(SUPABASE_KEY))
  # Build endpoint: return at most 1 row for this pid
  base <- paste0(SUPABASE_URL, "/rest/v1/", SUPABASE_TABLE)
  url  <- httr::modify_url(
    base,
    query = list(
      participant_id = paste0("eq.", pid),
      select = "participant_id",
      limit  = 1
    )
  )
  # Headers
  hdrs <- httr::add_headers(
    `apikey`         = SUPABASE_KEY,
    `Accept`         = "application/json",
    `Accept-Profile` = "public"
  )
  if (!is.null(access_token) && nzchar(access_token)) {
    hdrs <- httr::add_headers(
      `apikey`         = SUPABASE_KEY,
      `Accept`         = "application/json",
      `Accept-Profile` = "public",
      `Authorization`  = paste("Bearer", access_token)
    )
  }
  
  res <- httr::GET(url, hdrs)
  status <- httr::status_code(res)
  if (status >= 200 && status < 300) {
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    if (!nzchar(txt)) return(FALSE)
    dat <- jsonlite::fromJSON(txt)
    # dat can be a dataframe or list; treat any non-empty result as "exists"
    return(NROW(dat) >= 1)
  } else {
    # If the check fails (network, auth), propagate an error so caller can handle
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    stop(sprintf("Supabase check failed (%s): %s", status, txt))
  }
}
# -----------------------------
# Vignette generators and text
# -----------------------------
generate_balanced_vignettes <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pick <- data.frame(
    provider   = c("Doctor","Doctor","AI","AI"),
    severity   = c("Low","High","Low","High"),
    advice_len = c("Brief","Long","Long","Brief"),
    stringsAsFactors = FALSE
  )
  pick <- pick[sample(1:4), ]
  pick$vignette_id <- seq_len(4)
  pick
}

condition_text <- function(severity) {
  if (severity == "Low") {
    list(
      title = "Skin Rash",
      description = paste(
        "Imagine that over the past two days, you have found that you have developed a red rash on your upper left forearm. While it is unpleasantly itchy and unsightly, you are glad that it does not seem to be spreading. Moreover, aside from the rash itself, you feel fine and are not experiencing any other symptoms. Specifically, you do not have a fever and there is no blistering."
      )
    )
  } else {
    list(
      title = "Chest Pain",
      description = paste(
        "Imagine that about 30 minutes ago while you were sitting on your couch at home resting and reading, you suddenly started feeling a tightness in your chest that feels like a heavy pressure positioned right in the centre of your chest. This sensation has not gone away, and you are also finding it difficult to breathe. In addition, you have observed that you are sweating more than normal."
      )
    )
  }
}

provider_text <- function(provider) {
  if (provider == "Doctor") {
    "You have been directed to be consulted by a human clinician (a licensed medical doctor). The following advice has been provided by Dr. Tan Wei Min, MBBS (Family Medicine). [👨‍⚕️]"
  } else {
    "You have been directed to be consulted by an AI system designed to provide medical guidance. The following advice has been provided by HealthAI Virtual Clinician (LLM-v4). [🤖]"
  }
}

advice_text <- function(severity, advice_len) {
  if (severity == "Low" && advice_len == "Brief") {
    paste(
      "Based on your description, this presentation is consistent with simple contact or irritant dermatitis. Lukewarm short showers and using a bland moisturizer at least twice daily are recommended. Avoid known irritants and allergens. Continue to monitor the rash for the next week and watch for signs of infection (pus, honey-colored crusts, increasing pain, and fever) and seek care if present. If this does not improve after a week or two, arrange for a visit to a dermatologist."
    )
  } else if (severity == "Low" && advice_len == "Long") {
    paste(
      "Based on your description, this presentation is consistent with simple contact or irritant dermatitis. Lukewarm short showers and using a bland moisturizer at least twice daily are recommended. Avoid known irritants and allergens. Continue to monitor the rash for the next week and watch for signs of infection (pus, honey-colored crusts, increasing pain, and fever) and seek care if present. If this does not improve after a week or two, arrange for a visit to a dermatologist.", 
      
      "The guidance that was provided is consistent with the recommendations made by dermatology groups (including the American Academy of Dermatology) and the Singapore Ministry of Health primary-care guidelines for simple localized rashes. Atopic eczema has a prevalence rate in adults of about 10% and contact dermatitis is common, while serious systemic causes are uncommon in outpatient cohorts (<1%). Therefore, in the absence of danger signs (spreading redness, severe pain, warmth/swelling, fever), the recommended course of action is self-management.", 
      
      "If symptoms do not improve over time, it is recommended to review recent potential exposures (including soaps, detergents, cosmetics, metals) and ask for patch testing.",
      sep = "\n\n"
    )
  } else if (severity == "High" && advice_len == "Brief") {
    paste(
      "Based on your symptoms, this may be a time-sensitive emergency. Do not wait - call 995 or go to the nearest emergency department immediately. Do not drive yourself; use an ambulance or get assistance. If symptoms worsen, stay on the line with emergency services and keep them updated. While waiting, stop activity, sit or lie with head elevated, avoid food and drink, and have a list of your medications, known conditions, and allergies to hand.  "
    )
  } else {
    paste(
      "Based on your symptoms, this may be a time-sensitive emergency. Do not wait - call 995 or go to the nearest emergency department immediately. Do not drive yourself; use an ambulance or get assistance. If symptoms worsen, stay on the line with emergency services and keep them updated. While waiting, stop activity, sit or lie with head elevated, avoid food and drink, and have a list of your medications, known conditions, and allergies to hand.",  
      
      "This guidance follows widely accepted emergency and cardiology recommendations, including those developed by the American Heart Association and the Singapore Ministry of Health. Chest tightness with sweating and shortness of breath are warning signs of possible heart or lung emergencies, including heart attacks. In such cases, timing is of the essence because prompt treatment can restore blood flow, prevent damage, and reduce the risk death. Although many causes of chest pain are non-cardiac in nature, urgent testing is nevertheless recommended as emergency assessment can shorten time-to-treatment by 5-15%.", 
      
      "After emergency evaluation and any treatment, it is recommended to arrange a follow up with a primary care practitioner or a cardiologist within a week to examine findings, medications, and any recommended further testing. Seek urgent care if chest pain or tightness recurs, if symptoms worsen, you feel faint, or you experience a new shortness of breath.",
      sep = "\n\n"
    )
  }
}

summary_bullets <- function(severity) {
  if (severity == "Low") {
    c("Localised itchy rash, not spreading", "No fever or blistering; otherwise well")
  } else {
    c("Chest tightness with sweating", "Shortness of breath; persistent symptoms")
  }
}

# Provider icon (emoji or simple symbol)
provider_icon <- function(provider) {
  if (provider == "Doctor") {
    tags$span(class = "sticky-provider-icon", "👨‍⚕️")
  } else {
    tags$span(class = "sticky-provider-icon", "🤖")
  }
}

# -----------------------------
# Theme and CSS (Typeform-inspired)
# -----------------------------
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = "'TWKLausanne 400', Arial, sans-serif",
  heading_font = "'Tobias', Arial, sans-serif",
  primary = "#6C5CE7"
)

css <- "
/* Fonts */
@font-face {
  font-family: 'TWKLausanne 400';
  src: url('https://cdn.prod.website-files.com/66ffe2174aa8e8d5661c2708/683ffc9a2aa1aa6187af000c_TWKLausanne-400.woff') format('woff');
  font-weight: 400; font-style: normal; font-display: swap;
}
@font-face {
  font-family: 'Tobias';
  src: url('https://cdn.prod.website-files.com/66ffe2174aa8e8d5661c2708/67f7e38acef4cde59aad2dbe_Tobias-Regular.woff2') format('woff2');
  font-weight: 400; font-style: normal; font-display: swap;
}

/* Base */
html, body { background: #0f172a; color: #e5e7eb;}
.fullscreen { display: flex; align-items: center; justify-content: center; padding: 4vh 4vw; }

/* Desktop only: allow the outer card to scroll */
@media (min-width: 769px) {

    html, body { height: 100%;}
    .fullscreen { min-height: 100vh; }
    .card {
          max-height: calc(100vh - 8vh);
          overflow-y: auto;
          }
  #screen { overflow-y: auto; -webkit-overflow-scrolling: touch; }
}

.modal-content { background: #0f172a; color: #e5e7eb; }
.modal-title { font-size: 28px; font-weight: 800; margin-bottom: 8px; font-family: Tobias; color: #E7E393; }
.modal-body, .footer  { font-size: 16px; color: #E8D7F1; margin-bottom: 20px; line-height: 1.6; font-family: Arial, sans-serif; }
.footer {margin-top: 20px;}
.admin-panel {color: #e5e7eb}

/* Card canvas */
.card {
  width: min(960px, 92vw);
  background: rgba(17,24,39,0.72);
  border-radius: 18px;
  backdrop-filter: blur(12px);
  box-shadow: 0 24px 48px rgba(0,0,0,0.35);
  padding: 32px 36px; position: relative;
}

/* Top progress */
.top-progress { position: absolute; top: 0; left: 0; height: 4px; width: 100%; overflow: hidden; }
.top-progress-bar { height: 100%; width: 0%; background: linear-gradient(90deg, #6C5CE7, #00D4FF); transition: width .35s ease; }

/* Typo */
.title { font-size: 28px; font-weight: 800; margin-bottom: 8px; font-family: Tobias; color: #E7E393; }
.desc  { font-size: 16px; color: #E8D7F1; margin-bottom: 20px; line-height: 1.6; font-family: Arial, sans-serif; white-space: pre-line; overflow-wrap: anywhere; }
.label { font-weight: 600; color: #E8D7F1; margin: 10px 0 8px; }

.shiny-input-text, .shiny-input-password { font-family: Arial, sans-serif; }

/* Dots */
.progress-dots { display: flex; gap: 8px; margin-bottom: 16px; flex-wrap: wrap; }
.dot { width: 10px; height: 10px; border-radius: 50%; background: #334155; transition: transform .25s, background .25s; }
.dot.active { background: #6C5CE7; transform: scale(1.2); }
.dot.done   { background: #2EC4B6; }

/* Inputs */
textarea.form-control {
  background: rgba(2,6,23,0.6); color: #e5e7eb; border: 1px solid #334155;
  border-radius: 12px; padding: 12px; min-height: 140px;
}
textarea.form-control::placeholder { color: #64748b; }
textarea.form-control:focus { outline: none; box-shadow: 0 0 0 2px #6C5CE7; border-color: transparent; }
.shiny-options-group { color: #E8D7F1; }

/* Buttons */
.btn {
  border: none; border-radius: 12px; padding: 12px 18px; font-weight: 700;
  transition: filter .18s; font-family: Arial, sans-serif;
}
.btn:hover { filter: brightness(1.08); }
.btn-next { background: #2EC4B6; color: #0b1020; }
.btn-back { background: #6C5CE7; color: #e5e7eb; }
.btn-download { background: #2EC4B6; color: #0b1020; margin-bottom:10px;}
#restart {margin-bottom:10px;}
.btn.disabled, .btn:disabled { opacity: 0.6; cursor: not-allowed; }
.nav-actions { display: flex; gap: 12px; justify-content: space-between; margin-top: 16px; }
.nav-actions-right { display: flex; gap: 12px; }

/* Override Bootstrap variables for specific buttons */
.btn.btn-next {
  --bs-btn-bg: #10b981;
  --bs-btn-color: #0b1020;
  --bs-btn-hover-bg: #00F5F2 !important;
  --bs-btn-hover-color: #ffffff;
  --bs-btn-border-color: transparent;
  --bs-btn-hover-border-color: transparent;
  --bs-btn-focus-shadow-rgb: 16,185,129;
  --bs-btn-active-bg: #0f9e9b;
  --bs-btn-active-border-color: transparent;
}
.btn.btn-back {
  --bs-btn-bg: #475569;
  --bs-btn-color: #e5e7eb;
  --bs-btn-hover-bg: #A79EFF !important;
  --bs-btn-hover-color: #e5e7eb !important;
  --bs-btn-border-color: transparent;
  --bs-btn-hover-border-color: transparent;
  --bs-btn-focus-shadow-rgb: 71,85,105;
  --bs-btn-active-bg: #2b3443;
  --bs-btn-active-border-color: transparent;
}
.btn.btn-download {
  --bs-btn-bg: #00D4FF;
  --bs-btn-color: #0b1020;
  --bs-btn-hover-bg: #00F5F2 !important;
  --bs-btn-hover-color: #ffffff;
  --bs-btn-border-color: transparent;
  --bs-btn-hover-border-color: transparent;
  --bs-btn-focus-shadow-rgb: 0,212,255;
  --bs-btn-active-bg: #08a8c3;
  --bs-btn-active-border-color: transparent;
}

/* Modals: harmonise default button */
.modal-content .modal-footer .btn.btn-default {
  background-color: #6C5CE7 !important; color: #ffffff !important; border-color: transparent !important;
}
.modal-content .modal-footer .btn.btn-default:hover {
  background-color: #A79EFF !important; color: #ffffff !important; border-color: transparent !important;
}
.modal-content .modal-footer .btn:hover { filter: none !important; transform: none !important; }

/* Typeform scales */
.scale-group { display: grid; gap: 12px; }
.scale-2 { grid-template-columns: repeat(2, minmax(80px, 1fr)); }
.scale-3 { grid-template-columns: repeat(3, minmax(80px, 1fr)); }
.scale-5 { grid-template-columns: repeat(5, minmax(44px, 1fr)); }
.scale-6 { grid-template-columns: repeat(6, minmax(44px, 1fr)); }
.scale-7 { grid-template-columns: repeat(7, minmax(44px, 1fr)); }

.scale-item {
  background: rgba(17,24,39,0.6); border: 1px solid #334155; color: #E8D7F1;
  border-radius: 12px;   padding: clamp(6px, 1.6vw, 14px) clamp(4px, 1.2vw, 12px); text-align: center; font-weight: 700;
  user-select: none; cursor: pointer; transition: background .18s, border-color .18s;
  box-sizing: border-box; font-size: clamp(12px, 2vw, 16px); min-width: 0; ;

}
.main-pane { min-width: 0; }  /* ensures grid can compress inside pane */
.scale-item:hover { background: rgba(99,102,241,0.18); border-color: #6C5CE7; }
.scale-item.selected { background: #6C5CE7; border-color: #6C5CE7; color: #0b1020; }
.scale-item:focus { outline: 2px solid #6C5CE7; outline-offset: 2px; }

/* 2-flash animation */
@keyframes tf-flash-twice {
  0% { background-color: #6C5CE7; box-shadow: 0 0 0 0 rgba(108,92,231,0.00); }
  50% { background-color: #9E8CFF; box-shadow: 0 0 0 10px rgba(158,140,255,0.25); }
  100% { background-color: #6C5CE7; box-shadow: 0 0 0 0 rgba(108,92,231,0.00); }
}
.scale-item.flash { animation: tf-flash-twice 400ms ease-in-out 2 alternate; }

/* Sticky summary (desktop by default, mobile collapses to one column) */
.sticky-summary {
  background: rgba(17,24,39,0.72);
  border: 1px solid #334155;
  border-radius: 12px;
  padding: 12px 14px;
  backdrop-filter: blur(8px);
  box-shadow: 0 12px 24px rgba(0,0,0,0.30);
  color: #E8D7F1;
}
.sticky-summary-title { display: flex; align-items: center; gap: 8px; font-weight: 800; font-size: 18px; color: #E7E393; }

.sticky-summary-ul    { margin: 10px 0 0; padding-left: 18px; font-size: 13px; line-height: 1.4; }
.sticky-provider-icon { font-size: 24px; line-height: 1; }

/* Screen content layout: mobile first single column */
.screen-content { display: block; }
.main-pane { min-width: 0; } /* allow text to wrap in grid/flex */


/* Very small phones */
@media (max-width: 480px) {
  .full-screen {min-height: 80vh}
  .title { font-size: 20px; margin-bottom:0px; }
  .desc  { font-size: 16px; }
  .scale-wrap { --btn-min: 30px; }
  .scale-captions { font-size: 11px; }
  .form-control {font-size: 16px;}
  .btn {font-size: 15px; padding: 6px 9px;}
}

/* Desktop/Tablet: two-column layout and sticky aside */
@media (min-width: 769px) {
  .screen-content {
    display: grid;
    grid-template-columns: minmax(0, 1fr) 280px;
    gap: 16px; align-items: start;
  }
  .sticky-aside { width: auto; }
  .sticky-summary { position: sticky; top: 16px; max-width: 280px; }
}

/* Scale + captions wrapper */
.scale-wrap {
  width: 100%;
  display: grid;
  grid-template-rows: auto auto;       /* row 1 = buttons, row 2 = captions */
  row-gap: 6px;
  /* Button min-width is responsive via this variable */
  --btn-min: 44px;                      /* default */
}

/* Caption row: left/right labels aligned to edges of the button grid */
.scale-captions {
  display: grid;
  grid-template-columns: repeat(7, 1fr);
  column-gap: 8px;     /* match the buttons gap so edges line up */
  align-items: center;
  width: 100%;
  color: #E8D7F1;
  font-size: clamp(11px, 1.6vw, 13px);
  line-height: 1.3;
  margin: 0;           /* remove default margins that can nudge alignment */
  padding: 0;
}
/* Prevent long phrases from squishing; allow wrapping gracefully */
/* Left caption under column 1, left-justified */
.scale-caption-left {
  grid-column: 1;
  justify-self: start;
  text-align: left;
  margin: 0;
  margin-bottom: 2px;
}

/* Right caption under column 7, right-justified */
.scale-caption-right {
  grid-column: 7;
  justify-self: end;
  text-align: right;   /* makes “Very accurate” / “Completely trustworthy” hug the 7 */
  margin-right: 5px;
  margin-bottom: 2px;
}

/* Slide the whole card container (box) */
#appStage { position: relative; overflow: hidden; }
.card { will-change: transform, opacity; }

/* Ghost of outgoing card */
.card-ghost {
  position: absolute;
  inset: 0;
  width: 100%;
  z-index: 5;
  pointer-events: none;
}

/* Outgoing animations */
@keyframes card-out-left {
  from { opacity: 1; transform: translateX(0); }
  to   { opacity: 0; transform: translateX(-80px); }
}
@keyframes card-out-right {
  from { opacity: 1; transform: translateX(0); }
  to   { opacity: 0; transform: translateX(80px); }
}
.card-out-left  { animation: card-out-left 520ms ease both; }
.card-out-right { animation: card-out-right 520ms ease both; }

/* Incoming animations */
@keyframes card-in-right {
  from { opacity: 0; transform: translateX(80px); }
  to   { opacity: 1; transform: translateX(0); }
}
@keyframes card-in-left {
  from { opacity: 0; transform: translateX(-80px); }
  to   { opacity: 1; transform: translateX(0); }
}
.card-in-right { animation: card-in-right 520ms ease both; }
.card-in-left  { animation: card-in-left  520ms ease both; }

/* Mobile: outer card does not scroll; inner content does. Keep card compact when content is short. */
@media (max-width: 768px) {
  .sticky-summary{margin-top:10px;}
   .sticky-aside {
    display: none; /* Hide on mobile to save space */
  }
  
  /* FIXED: Don't center on mobile, just fill the space */
  .fullscreen { 
    min-height: 70vh; 
    display: block; /* Changed from flex */
    padding: 5px; 
    overflow: hidden; /* Prevent outer scroll */
  }
  
    /* Fixed card height - no centering */
  .card {
    width: calc(100vw - 50px); /* Account for 5px padding on each side */
    height: 80vh; /* Default fallback - JavaScript will override */
    max-height: 80vh; /* Default fallback - JavaScript will override */
    margin: 0 auto; /* Simple horizontal centering */
    display: flex;
    flex-direction: column;
    overflow: hidden;
    padding: 16px;
    box-sizing: border-box;
  }

  /* Wrapper does not scroll */
  #screen {
    flex: 1 1 auto;
    height: auto;
    overflow: hidden;
  }

  /* Flex chain must allow children to shrink so inner can scroll */
  .screen-content {
    display: flex;
    flex-direction: column;
    height: 100%;
    min-height: 0;                /* critical for inner scrolling */
  }
  .main-pane {
    display: flex;
    flex-direction: column;
    flex: 1 1 auto;
    min-height: 0;                /* critical for inner scrolling */
    max-height: calc(100vh - 120px); /* ADDED: ensure container has max height */

  }

  /* Advice: scroll only when long; otherwise compact (no hole) */
  .main-pane .desc { flex: 0 1 auto; overflow: visible; max-height: none; }
  .main-pane .desc.long {
    flex: 1 1 auto;
    min-height: 0;
    overflow-y: auto;
    -webkit-overflow-scrolling: touch;
    padding-right: 2px;
    padding-bottom: 12px;         /* clearance above buttons */
    max-height: calc(100vh - 300px); /* Adjust this value as needed */

  }

  /* 1–7 scale: scroll inside a capped area; fit columns */
  .main-pane .scale-wrap {
    flex: 0 1 auto;               /* don’t fill entire pane */
    max-height: calc(100vh - 250px);  /* ADDED: explicit max height */
    overflow-y: auto;
    -webkit-overflow-scrolling: touch;
    padding-right: 2px;
    padding-bottom: 12px;
  }
  .scale-group.scale-7 {
    display: grid;
    grid-template-columns: repeat(7, minmax(0, 1fr));
    column-gap: 8px;
    width: 100%;
  }
  .scale-group.scale-6 {
    display: grid;
    grid-template-columns: repeat(6, minmax(0, 1fr));
    column-gap: 8px;
    width: 100%;
  }
  .scale-item {
    min-width: 0;
    padding: 8px 6px;
    white-space: normal;
    font-size: 8px;
  }

  .nav-actions {
    flex: 0 0 auto; 
    margin-top: 8px; 
    background: rgba(15,23,42,0.9); 
    position: relative;
    z-index: 10;  
  }
  
  
/* Collapsible toggles with chevron */
.admin-toggle {
  display: flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
  user-select: none;
  font-weight: 700;
  color: #E7E393;
  text-decoration: none;
  padding: 8px 10px;
  border: 1px solid #334155;
  border-radius: 8px;
  background: rgba(17,24,39,0.6);
  margin-top: 10px;
}
.admin-toggle:hover { color: #A79EFF; }

.admin-toggle .chev {
  display: inline-block;
  transition: transform 0.2s ease;
  font-size: 14px;
  line-height: 1;
}
/* Rotate chevron when expanded (Bootstrap toggles .collapsed on the trigger) */
.admin-toggle:not(.collapsed) .chev { transform: rotate(90deg); }

/* Collapsed content containers */
.admin-panel, .admin-guard-panel {
  margin-top: 8px;
  padding: 12px;
  border: 1px solid #334155;
  border-radius: 10px;
  background: rgba(17,24,39,0.6);
}

/* Warning modal styling */
.warning-modal .modal-backdrop {
  background-color: rgba(0, 0, 0, 0.8) !important;
}

.warning-modal .modal-content {
  background: linear-gradient(135deg, #fbbf24, #f59e0b) !important;
  color: #1f2937 !important;
  border: 3px solid #d97706;
  box-shadow: 0 25px 50px rgba(217, 119, 6, 0.3);
  position: relative;
}

.warning-modal .modal-content::before {
  content: '🔒';
  position: absolute;
  top: -15px;
  right: -15px;
  font-size: 24px;
  background: #dc2626;
  color: white;
  width: 40px;
  height: 40px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  border: 3px solid white;
  box-shadow: 0 4px 8px rgba(0,0,0,0.3);
}

/* Only disable elements when WARNING modal is active */
body.warning-modal-active .card:not(.warning-modal) {
  pointer-events: none !important;
  opacity: 0.7;
}

/* Only disable elements when WARNING modal is active, and not in other modals */
body.warning-modal-active input:not(#warningOkBtn),
body.warning-modal-active button:not(#warningOkBtn),
body.warning-modal-active .scale-item {
  pointer-events: none !important;
  opacity: 0.3 !important;
  filter: grayscale(1) !important;
}

/* Exception: don't disable elements inside non-warning modals */
body.warning-modal-active .modal:not(.warning-modal) input,
body.warning-modal-active .modal:not(.warning-modal) button {
  pointer-events: auto !important;
  opacity: 1 !important;
  filter: none !important;
}

/* Character limit styling for post questionnaire */
#post_q_text {
  max-length: 2400; /* Roughly 400 words * 6 chars average */
}

.char-counter {
  font-size: 12px;
  color: #64748b;
  text-align: right;
  margin-top: 4px;
}

.char-counter.warning {
  color: #f59e0b;
}

.char-counter.error {
  color: #ef4444;
}

/* Admin download button styling */
.download-grid {
  margin-top: 15px;
}

.download-grid .btn {
  width: 100%;
  text-align: center;
  font-size: 13px;
}

.btn.btn-download {
  --bs-btn-bg: #00D4FF;
  --bs-btn-color: #0b1020;
  --bs-btn-hover-bg: #00F5F2 !important;
  --bs-btn-hover-color: #ffffff;
  margin-bottom: 0px;
}
"

# -----------------------------
# UI
# -----------------------------
ui <- page_fluid(
  theme = theme,
  tags$head(
    tags$title("Health Vignette Questionnaire"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "Participate in our questionnaire experience. Your feedback helps advance behavioral health research."),
    # Open Graph / Facebook / Teams / WhatsApp
    tags$meta(property = "og:title", content = "Health Vignette Questionnaire"),
    tags$meta(property = "og:description", content = "Participate in our questionnaire experience. Your feedback helps advance behavioral health research."),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = "https://bzadoroz.shinyapps.io/ShinyVignettesv2/"),
    tags$meta(property = "og:image", content = "https://bzadoroz.shinyapps.io/ShinyVignettesv2/splash.jpg"), 
    
    # Twitter Card
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Health Vignette Questionnaire"),
    tags$meta(name = "twitter:description", content = "Participate in our questionnaire experience. Your feedback helps advance behavioral health research."),
    tags$meta(name = "twitter:image", content = "https://bzadoroz.shinyapps.io/ShinyVignettesv2/splash.jpg"), 
    
    tags$script(src = "app.js")
  ),
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon-ecg.svg")
  ),
  tags$head(tags$style(HTML(css))),
  div(id = "appStage", class = "fullscreen",
      div(class = "card gallery js-flickity",
          div(class = "top-progress", div(id = "topProgressBar", class = "top-progress-bar")),
          uiOutput("progressUI"),
          uiOutput("screenUI")
      )
  ))
# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  TOTAL_STEPS <- 45L # 12 pre + 4 vignettes * 8 steps (Narrative + Expl1 + Expl2 + Q1 + Q2 + Q3 + Q4 + Q5) + post-experience
  PRE_QUESTIONNAIRE_STEPS <- 12L
  VIGNETTE_START_STEP <- 13L
  
  QUESTIONNAIRE_MAP <- list(
    "intro" = 1L,
    "demographics" = 2L,
    "crt_intro" = 3L,
    "crt" = 4L,
    "gaais_intro" = 5L,
    "gaais" = 6L,
    "fwa_intro" = 7L,
    "fwa" = 8L,
    "teique_intro" = 9L,
    "teique" = 10L,
    "mhlc_intro" = 11L,
    "mhlc" = 12L
  )
  
  rv <- reactiveValues(
    admin_skip_used = FALSE,
    
    started = FALSE,
    pre_step = 0L,
    
    section_started_at = list(
      demographics = as.POSIXct(NA_character_),
      crt         = as.POSIXct(NA_character_),
      gaais       = as.POSIXct(NA_character_),
      fwa         = as.POSIXct(NA_character_),
      teique      = as.POSIXct(NA_character_),
      mhlc        = as.POSIXct(NA_character_)
    ),
    
    pre_q_completed = list(
      demographics = numeric(),
      crt = numeric(),
      gaais = numeric(),
      fwa = numeric(),
      teique = numeric(),
      mhlc = numeric()
    ),
    
    
    # Popped answers
    popped_answers = list(
      demographics = list(), crt = list(),
      gaais = list(), fwa = list(),
      teique = list(), mhlc = list()
    ),
    
    
    # Pre-questionnaire data
    pre_answers = list(
      demographics = list(),
      crt = list(),
      gaais = list(),
      fwa = list(),
      teique = list(),
      mhlc = list()
    ),
    
    # Track completion status
    completed_questionnaires = character(),
    
    # Vignette data
    vignettes = NULL,
    current_vignette = 0L,  # 1..4
    vig_step = 0L,      # 1..8
    answers = vector("list", 4),
    responses = data.frame(
      participant_id = character(),
      vignette_number = integer(),
      provider = character(),
      severity = character(),
      advice_len = character(),
      q_take_advice = logical(),         # TRUE/FALSE
      q_perceived_accuracy = integer(),
      q_trustworthiness = integer(),   # remapped to int
      q_concern_condition = integer(),       # NEW Q4
      q_concern_circumstances = integer(),   # NEW Q5
      post_vignette_response = character(),  # post vignette
      started_at = as.POSIXct(character()),
      submitted_at = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    post_vignette_answer = NULL,
    last_dir = "forward",
    sb_access_token = NULL,
    sb_refresh_token = NULL,
    sb_expires_at    = NULL,
    sb_auth_attempted = FALSE,
    sb_auth_error     = NULL,
    sb_auth_tries     = 0L,
    sb_user_email = NULL,
    # NEW: store start time per vignette
    vignette_started_at = as.POSIXct(rep(NA_character_, 4))
    
    
  )
  
  show_blocking_warning <- function(title, message, duration = 5) {
    session$sendCustomMessage('showBlockingWarning', list(
      title = title,
      message = message,
      duration = duration
    ))
  }
  
  rv$warning_cooldowns <- list(
    gaais_straightline = as.POSIXct(NA),
    teique_straightline = as.POSIXct(NA),
    mhlc_straightline = as.POSIXct(NA),
    vignette_pattern = as.POSIXct(NA),
    speed_warning = as.POSIXct(NA),
    attention_check = as.POSIXct(NA)
  )
  rv$warning_shown <- list(
    gaais_straightline = FALSE,
    teique_straightline = FALSE,
    mhlc_straightline = FALSE,
    vignette_pattern = FALSE,
    speed_warning = FALSE
  )
  
  accuracy_levels <- c(
    "Very inaccurate",
    "Inaccurate",
    "Somewhat inaccurate",
    "Neither inaccurate nor accurate",
    "Somewhat accurate",
    "Accurate",
    "Very accurate"
  )
  
  trust_levels <- c(
    "Not trustworthy at all",
    "Untrustworthy",
    "Somewhat untrustworthy",
    "Neutral",
    "Somewhat trustworthy",
    "Trustworthy",
    "Fully trustworthy"
  )
  
  # Demographics options
  demographics_questions <- list(
    age = list(
      question = "To which age group do you belong?",
      type = "scale",
      options = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ),
    gender = list(
      question = "What is your gender?",
      type = "scale", 
      options = c("Male", "Female", "Non-binary/Other")
    ),
    education = list(
      question = "What is the highest level of education that you have attained?",
      type = "scale",
      options = c("High school or less", "Some college", "Bachelor's", "Master's", "Doctoral", "Post-doctoral or beyond")
    ),
    postcode = list(
      question = "What are the first two digits of your postcode?",
      type = "text",
      validation = "postcode"
    )
  )
  
  crt_questions <- list(
    q1 = "If you’re running a race and you pass the person in second place, what place are you in?",
    q2 = "A farmer had 15 sheep and all but 8 died. How many are left?",
    q3 = "Emily’s father has three daughters. The first two are named April and May. What is the third daughter’s name?",
    q4 = "How many cubic feet of dirt are there in a hole that is 3’ deep x 3’ wide x 3’ long?"
  )
  
  gaais_questions <- c(
    "For routine transactions, I would rather interact with an artificially intelligent system than with a human.",
    "Artificial Intelligence can provide new economic opportunities for this country.", 
    "Organisations use Artificial Intelligence unethically.",
    "Artificially intelligent systems can help people feel happier.",
    "I am impressed by what Artificial Intelligence can do.",
    "I think artificially intelligent systems make many errors. ",
    "I am interested in using artificially intelligent systems in my daily life.",
    "I find Artificial Intelligence sinister. ",
    "Artificial Intelligence might take control of people.",
    "I think Artificial Intelligence is dangerous. ",
    "Artificial Intelligence can have positive impacts on people’s wellbeing.",
    "Artificial Intelligence is exciting. ",
    "I would be grateful if you could select Strongly Agree. ",
    "An artificially intelligent agent would be better than an employee in many routine jobs.",
    "There are many beneficial applications of Artificial Intelligence. ",
    "I shiver with discomfort when I think about future uses of Artificial Intelligence. ",
    "Artificially intelligent systems can perform better than humans.",
    "Much of society will benefit from a future full of Artificial Intelligence.",
    "I would like to use Artificial Intelligence in my own job. ",
    "People like me will suffer if Artificial Intelligence is used more and more. ",
    "Artificial Intelligence is used to spy on people."
  )
  
  fwa_questions <- c(
    "How frequently do you use artificial intelligence (AI) tools in general?",
    "How frequently do you use artificial intelligence (AI) tools specifically in the medical context?"
  )
  
  teique_questions <- c(
    "Expressing my emotions with words is not a problem for me.",
    "I often find it difficult to see things from another person’s viewpoint.",
    "On the whole, I’m a highly motivated person.",
    "I usually find it difficult to regulate my emotions. ",
    "I generally don’t find life enjoyable. ",
    "I can deal effectively with people.",
    "I tend to change my mind frequently. ",
    "Many times, I can’t figure out what emotion I'm feeling.",
    "I feel that I have a number of good qualities.",
    "I often find it difficult to stand up for my rights.",
    "I’m usually able to influence the way other people feel.",
    "On the whole, I have a gloomy perspective on most things.",
    "Those close to me often complain that I don’t treat them right.",
    "I often find it difficult to adjust my life according to the circumstances.",
    "On the whole, I’m able to deal with stress.",
    "I often find it difficult to show my affection to those close to me.",
    "I’m normally able to “get into someone’s shoes” and experience their emotions.",
    "I normally find it difficult to keep myself motivated.",
    "I’m usually able to find ways to control my emotions when I want to.",
    "On the whole, I’m pleased with my life.",
    "I would describe myself as a good negotiator.",
    "I tend to get involved in things I later wish I could get out of. ",
    "I often pause and think about my feelings.",
    "I believe I’m full of personal strengths.",
    "I tend to “back down” even if I know I’m right. ",
    "I don’t seem to have any power at all over other people’s feelings. ",
    "I generally believe that things will work out fine in my life.",
    "I find it difficult to bond well even with those close to me. ",
    "Generally, I’m able to adapt to new environments.",
    "Others admire me for being relaxed.",
    "I would be grateful if you selected the Completely Disagree option."  # NEW - attention check
  )
  
  mhlc_questions <- c(
    "If I get sick, it is my own behavior which determines how soon I get well again. ",
    "No matter what I do, if I am going to get sick, I will get sick. ",
    "Having regular contact with my physician is the best way for me to avoid illness. ",
    "Most things that affect my health happen to me by accident.",
    "Whenever I don't feel well, I should consult a medically trained professional.",
    "I am in control of my health.",
    "My family has a lot to do with my becoming sick or staying healthy. ",
    "When I get sick, I am to blame.",
    "Luck plays a big part in determining how soon I will recover from an illness.",
    "Health professionals control my health.",
    "My good health is largely a matter of good fortune.",
    "The main thing which affects my health is what I myself do.",
    "If I take care of myself, I can avoid illness.",
    "When I recover from an illness, it's usually because other people (for example, doctors, nurses, family, friends) have been taking good care of me. ",
    "No matter what I do, I am likely to get sick.",
    "If it's meant to be, I will stay healthy.",
    "If I take the right actions, I can stay healthy.",
    "Regarding my health, I can only do what my doctor tells me to do."
  )
  
  # Enter input error handler (corrected and structured)
  observeEvent(input$enter_blocked, {
    # 1) Vignette validation (steps 4..8 are question pages)
    if (rv$current_vignette >= 1 && rv$current_vignette <= 4) {
      if (rv$vig_step == 4L) {
        showModal(modalDialog(
          title = "Incomplete Response",
          "Please select Yes or No.",
          easyClose = TRUE
        ))
      } else if (rv$vig_step %in% c(5L, 6L, 7L, 8L)) {
        showModal(modalDialog(
          title = "Incomplete Response",
          "Please select a value 1–7.",
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Action blocked",
          "You can only use Enter to advance on the current screen.",
          easyClose = TRUE
        ))
      }
      return()
    }
    
    # 2) Pre-questionnaire validation (even-numbered steps with inputs)
    if (rv$current_vignette == 0 && rv$pre_step >= 1 && rv$pre_step <= 12) {
      if (rv$pre_step == 2L) {
        # Demographics
        current_q_num <- length(rv$pre_answers$demographics) + 1
        q_names <- names(demographics_questions)
        current_q_name <- q_names[current_q_num]
        q_data <- demographics_questions[[current_q_name]]
        
        if (q_data$type == "text") {
          # Postcode validation
          answer <- trimws(input$current_answer %||% "")
          if (!nzchar(answer)) {
            showModal(modalDialog(
              title = "Incomplete Response",
              "Please enter the first two digits of your postcode.",
              easyClose = TRUE
            ))
          } else if (!grepl("^[0-9]{2}$", answer)) {
            showModal(modalDialog(
              title = "Invalid Input",
              "Please enter exactly 2 digits (numbers only).",
              easyClose = TRUE
            ))
          }
        } else {
          showModal(modalDialog(
            title = "Incomplete Response",
            "Please select an option.",
            easyClose = TRUE
          ))
        }
        
      } else if (rv$pre_step == 4L) {
        # CRT - text must be filled
        answer <- trimws(input$current_answer %||% "")
        if (!nzchar(answer)) {
          showModal(modalDialog(
            title = "Incomplete Response",
            "Please provide an answer.",
            easyClose = TRUE
          ))
        }
        
      } else if (rv$pre_step == 6L) {
        # GAAIS - scale selection required
        answer <- input$gaais_q %||% ""
        if (!nzchar(answer)) {
          showModal(modalDialog(
            title = "Incomplete Response",
            "Please select a rating.",
            easyClose = TRUE
          ))
        }
        
      } else if (rv$pre_step == 8L) {
        # FWA
        answer <- input$fwa_q %||% ""
        if (!nzchar(answer)) {
          showModal(modalDialog(
            title = "Incomplete Response",
            "Please select a rating.",
            easyClose = TRUE
          ))
        }
        
      } else if (rv$pre_step == 10L) {
        # TEIQue
        answer <- input$teique_q %||% ""
        if (!nzchar(answer)) {
          showModal(modalDialog(
            title = "Incomplete Response",
            "Please select a rating.",
            easyClose = TRUE
          ))
        }
        
      } else if (rv$pre_step == 12L) {
        # MHLC
        answer <- input$mhlc_q %||% ""
        if (!nzchar(answer)) {
          showModal(modalDialog(
            title = "Incomplete Response",
            "Please select a rating.",
            easyClose = TRUE
          ))
        }
        
      } else {
        # Intro screens (1, 3, 5, 7, 9, 11) have no validation
        showModal(modalDialog(
          title = "Action blocked",
          "You can only use Enter to advance on the current screen.",
          easyClose = TRUE
        ))
      }
      return()
    }
    
    # 3) Fallback
    showModal(modalDialog(
      title = "Action blocked",
      "You can only use Enter to advance on the current screen.",
      easyClose = TRUE
    ))
  })
  
  
  # Additional Supabase helpers
  
  
  
  # Machine user sign-in and token refresh
  # Try to sign the machine user in; returns TRUE on success, FALSE otherwise
  machine_sign_in <- function() {
    rv$sb_auth_attempted <- TRUE
    rv$sb_auth_tries <- rv$sb_auth_tries + 1L
    
    # Explicit checks instead of req()
    if (!nzchar(APP_EMAIL) || !nzchar(APP_PASSWORD)) {
      rv$sb_auth_error <- "Missing SUPABASE_APP_EMAIL or SUPABASE_APP_PASSWORD"
      message(rv$sb_auth_error)
      return(FALSE)
    }
    if (!nzchar(SUPABASE_URL) || !nzchar(SUPABASE_KEY)) {
      rv$sb_auth_error <- "Missing SUPABASE_URL or SUPABASE_KEY"
      message(rv$sb_auth_error)
      return(FALSE)
    }
    
    # Attempt sign-in
    tryCatch({
      tok <- supabase_sign_in(APP_EMAIL, APP_PASSWORD)
      rv$sb_access_token  <- tok$access_token
      rv$sb_refresh_token <- tok$refresh_token
      rv$sb_expires_at    <- Sys.time() + as.numeric(tok$expires_in)
      rv$sb_auth_error    <- NULL
      #      message("Machine user signed in; token acquired.")
      TRUE
    }, error = function(e) {
      rv$sb_auth_error <- paste("Machine user sign-in error:", e$message)
      message(rv$sb_auth_error)
      FALSE
    })
  }
  
  # Kick off sign-in and retry up to, say, 10 times (every 5 seconds) until it succeeds
  # Only attempt machine sign-in if not using admin skip
  observe({
    # Skip machine auth if admin skip is being used
    if (isTRUE(rv$admin_skip_used)) return()
    
    if (is.null(rv$sb_access_token) || !nzchar(rv$sb_access_token)) {
      invalidateLater(5000, session)
      if (rv$sb_auth_tries < 10L) {
        machine_sign_in()
      } else {
        # Only show this message if not using admin skip
        if (!isTRUE(rv$admin_skip_used)) {
          message("Machine user sign-in: reached max retries.")
        }
      }
    }
  }, label = "machine_sign_in_retry")
  
  
  # Admin state
  rv$admin_unlocked <- FALSE
  rv$sim_rows <- NULL
  
  # Unlock admin 
  observeEvent(input$admin_unlock, {
    key <- trimws(input$admin_key %||% "")
    if (identical(key, "H1ghlyS3cure!")) {
      rv$admin_unlocked <- TRUE
      showNotification("Admin unlocked.", type = "message", duration = 3)
    } else {
      showModal(modalDialog(title = "Invalid Admin Key",
                            "The key you entered is incorrect.", easyClose = TRUE))
    }
  })
  
  # Static (non-collapsible) Admin tools shown after unlock
  output$adminPanel <- renderUI({
    if (!isTRUE(rv$admin_unlocked)) return(NULL)  # This line was the issue
    
    div(
      class = "admin-panel",
      
      # NEW: Skip Pre-questionnaire section
      div(class = "label", "Development Tools"),
      div(class = "nav-actions",
          actionButton("btn_skip_pre", "Skip to Vignettes", class = "btn btn-back"),
          actionButton("btn_skip_to_post", "Skip to Post-Question", class = "btn btn-back")
      ),
      
      # Existing simulation section
      div(class = "label", style = "margin-top: 20px;", "Simulate Participants"),
      numericInput("sim_n", "Number of participants",
                   value = 100, min = 1, step = 50, width = "240px"),
      numericInput("sim_seed", "Random seed (optional)",
                   value = NA, min = 1, step = 1, width = "240px"),
      div(class = "nav-actions",
          actionButton("btn_simulate", "Simulate", class = "btn btn-back")
      ),
      
      # Download buttons in a clean grid
      div(class = "download-grid", style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-top: 15px;",
          downloadButton("download_sim_csv", "Download All Data", class = "btn btn-download"),
          downloadButton("download_sim_pre", "Download Pre-questionnaire", class = "btn btn-download")
      ),
      div(style = "margin-top: 10px;",
          downloadButton("download_sim_vignettes", "Download Vignettes Only", class = "btn btn-download", style = "width: 100%;")
      ),
      uiOutput("simSummary")
    )
  })
  
  
  # Download pre-questionnaire data only
  output$download_sim_pre <- downloadHandler(
    filename = function() {
      paste0("simulated_prequestionnaire_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      dat <- rv$sim_rows
      if (is.null(dat) || !NROW(dat)) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      # Filter to pre-questionnaire data only
      pre_dat <- dat[dat$vignette_number == 0, ]
      
      # Fix Excel postcode issue on export only
      pre_dat$post_vignette_response <- ifelse(
        grepl("DEMOGRAPHICS_postcode", pre_dat$severity) & 
          grepl("^[0-9]{2}$", pre_dat$post_vignette_response),
        paste0("'", pre_dat$post_vignette_response),
        pre_dat$post_vignette_response
      )
      
      write.csv(pre_dat, file, row.names = FALSE)
    }
  )
  
  # Download vignette data only
  output$download_sim_vignettes <- downloadHandler(
    filename = function() {
      paste0("simulated_vignettes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      dat <- rv$sim_rows
      if (is.null(dat) || !NROW(dat)) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      # Filter to vignette data only (vignette_number 1-4 and 99 for post-vignette)
      vig_dat <- dat[dat$vignette_number >= 1, ]
      write.csv(vig_dat, file, row.names = FALSE)
    }
  )
  
  # Random generators for responses (tune as desired)
  rand_yes_no <- function(p_yes = 0.6) if (runif(1) < p_yes) "Yes" else "No"
  rand_scale7 <- function(center = 5, spread = 1.3) {
    v <- round(rnorm(1, mean = center, sd = spread))
    max(1, min(7, v))
  }
  rand_scale5 <- function(center = 3, spread = 1.0) {
    v <- round(rnorm(1, mean = center, sd = spread))
    max(1, min(5, v))
  }
  rand_scale6 <- function(center = 3.5, spread = 1.2) {
    v <- round(rnorm(1, mean = center, sd = spread))
    max(1, min(6, v))
  }
  
  
  # Simulate n participants with complete pre-questionnaire and vignette data
  simulate_participants <- function(n, seed = NULL) {
    if (!is.null(seed) && !is.na(seed)) set.seed(as.integer(seed))
    
    all_rows <- vector("list", n * (4 + 80))  # 4 vignettes + ~80 pre-questionnaire items
    idx <- 0L
    
    for (k in seq_len(n)) {
      pid <- sprintf("sim_%06d", k)
      
      # Random base time within last 24 hours
      base_t <- Sys.time() - runif(1, min = 3600, max = 24 * 3600)
      current_time <- base_t
      
      # === SIMULATE PRE-QUESTIONNAIRE DATA ===
      
      # 1. Demographics (4 questions)
      demo_start <- current_time
      current_time <- current_time + runif(1, 30, 120)  # 30-120 seconds
      
      # Age (1-7)
      age_val <- sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+", "<18"), 1, 
                        prob = c(0.25, 0.25, 0.20, 0.15, 0.10, 0.04, 0.01))
      idx <- idx + 1L
      all_rows[[idx]] <- data.frame(
        participant_id = pid, vignette_number = 0L, provider = "PRE_DEMOGRAPHICS",
        severity = "DEMOGRAPHICS_age", advice_len = "DEMOGRAPHICS",
        q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
        q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
        post_vignette_response = age_val, started_at = demo_start, submitted_at = current_time,
        stringsAsFactors = FALSE
      )
      
      # Gender (1-3)
      gender_val <- sample(c("Male", "Female", "Non-binary/Other"), 1, prob = c(0.45, 0.50, 0.05))
      idx <- idx + 1L
      all_rows[[idx]] <- data.frame(
        participant_id = pid, vignette_number = 0L, provider = "PRE_DEMOGRAPHICS",
        severity = "DEMOGRAPHICS_gender", advice_len = "DEMOGRAPHICS",
        q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
        q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
        post_vignette_response = gender_val, started_at = demo_start, submitted_at = current_time,
        stringsAsFactors = FALSE
      )
      
      # Education (1-6)
      edu_val <- sample(c("High school or less", "Some college", "Bachelor's", "Master's", "Doctoral", "Post-doctoral or beyond"), 
                        1, prob = c(0.15, 0.25, 0.35, 0.20, 0.04, 0.01))
      idx <- idx + 1L
      all_rows[[idx]] <- data.frame(
        participant_id = pid, vignette_number = 0L, provider = "PRE_DEMOGRAPHICS",
        severity = "DEMOGRAPHICS_education", advice_len = "DEMOGRAPHICS",
        q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
        q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
        post_vignette_response = edu_val, started_at = demo_start, submitted_at = current_time,
        stringsAsFactors = FALSE
      )
      
      # Postcode (2 digits)
      postcode_val <- sprintf("%02d", sample(1:80, 1))  # Clean data, no prefix
      idx <- idx + 1L
      all_rows[[idx]] <- data.frame(
        participant_id = pid, vignette_number = 0L, provider = "PRE_DEMOGRAPHICS",
        severity = "DEMOGRAPHICS_postcode", advice_len = "DEMOGRAPHICS",
        q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
        q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
        post_vignette_response = postcode_val, started_at = demo_start, submitted_at = current_time,
        stringsAsFactors = FALSE
      )
      
      # 2. CRT (4 questions)
      crt_start <- current_time + runif(1, 10, 60)
      current_time <- crt_start
      
      crt_answers <- list(
        sample(1:9, 1),  # Q1: single digit
        sample(1:9, 1),  # Q2: single digit  
        sample(c("Emily", "April", "May", "June", "Lisa"), 1),  # Q3: name
        sample(c("0", "27", "5"), 1)  # Q4: typical answers
      )
      
      for (i in 1:4) {
        idx <- idx + 1L
        all_rows[[idx]] <- data.frame(
          participant_id = pid, vignette_number = 0L, provider = "PRE_CRT",
          severity = paste0("CRT_q", i), advice_len = "CRT",
          q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
          q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
          post_vignette_response = as.character(crt_answers[[i]]), 
          started_at = crt_start, submitted_at = current_time,
          stringsAsFactors = FALSE
        )
        current_time <- current_time + runif(1, 5, 30)
      }
      
      # 3. GAAIS (21 questions)
      gaais_start <- current_time + runif(1, 10, 60)
      current_time <- gaais_start
      
      # Simulate somewhat consistent GAAIS responses (with some variation)
      base_attitude <- sample(c(2, 3, 4), 1, prob = c(0.3, 0.4, 0.3))  # Slightly negative to neutral
      
      for (i in 1:21) {
        # Add some variation around base attitude
        response <- pmax(1, pmin(5, base_attitude + sample(-1:1, 1, prob = c(0.2, 0.6, 0.2))))
        
        # Question 13 is attention check - should be "5" (Strongly Agree)
        if (i == 13) response <- 5
        
        idx <- idx + 1L
        all_rows[[idx]] <- data.frame(
          participant_id = pid, vignette_number = 0L, provider = "PRE_GAAIS",
          severity = paste0("GAAIS_q", i), advice_len = "GAAIS",
          q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
          q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
          post_vignette_response = as.character(response),
          started_at = gaais_start, submitted_at = current_time,
          stringsAsFactors = FALSE
        )
        current_time <- current_time + runif(1, 2, 15)
      }
      
      # 4. FWA (2 questions)
      fwa_start <- current_time + runif(1, 5, 30)
      current_time <- fwa_start
      
      for (i in 1:2) {
        # FWA responses tend to be lower (1-3 mostly)
        response <- sample(1:5, 1, prob = c(0.4, 0.3, 0.2, 0.08, 0.02))
        
        idx <- idx + 1L
        all_rows[[idx]] <- data.frame(
          participant_id = pid, vignette_number = 0L, provider = "PRE_FWA",
          severity = paste0("FWA_q", i), advice_len = "FWA",
          q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
          q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
          post_vignette_response = as.character(response),
          started_at = fwa_start, submitted_at = current_time,
          stringsAsFactors = FALSE
        )
        current_time <- current_time + runif(1, 3, 20)
      }
      
      # 5. TEIQue (31 questions including attention check)
      teique_start <- current_time + runif(1, 10, 60)
      current_time <- teique_start
      
      # Simulate personality-based responses
      base_ei <- sample(3:6, 1)  # Base emotional intelligence level
      
      for (i in 1:31) {
        if (i == 31) {
          # Attention check - should be "1" (Completely Disagree)
          response <- 1
        } else {
          # Add variation around base EI level
          response <- pmax(1, pmin(7, base_ei + sample(-2:2, 1, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))))
        }
        
        idx <- idx + 1L
        all_rows[[idx]] <- data.frame(
          participant_id = pid, vignette_number = 0L, provider = "PRE_TEIQUE",
          severity = paste0("TEIQUE_q", i), advice_len = "TEIQUE",
          q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
          q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
          post_vignette_response = as.character(response),
          started_at = teique_start, submitted_at = current_time,
          stringsAsFactors = FALSE
        )
        current_time <- current_time + runif(1, 2, 12)
      }
      
      # 6. MHLC (18 questions)
      mhlc_start <- current_time + runif(1, 10, 60)
      current_time <- mhlc_start
      
      # Health locus of control varies by person
      base_control <- sample(2:5, 1)  # Base control belief
      
      for (i in 1:18) {
        response <- pmax(1, pmin(6, base_control + sample(-1:1, 1, prob = c(0.25, 0.5, 0.25))))
        
        idx <- idx + 1L
        all_rows[[idx]] <- data.frame(
          participant_id = pid, vignette_number = 0L, provider = "PRE_MHLC",
          severity = paste0("MHLC_q", i), advice_len = "MHLC",
          q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
          q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
          post_vignette_response = as.character(response),
          started_at = mhlc_start, submitted_at = current_time,
          stringsAsFactors = FALSE
        )
        current_time <- current_time + runif(1, 2, 15)
      }
      
      # === SIMULATE VIGNETTE DATA ===
      vset <- generate_balanced_vignettes()
      current_time <- current_time + runif(1, 60, 180)  # Break before vignettes
      
      for (i in seq_len(nrow(vset))) {
        v <- vset[i, ]
        
        # Make distributions slightly depend on provider/severity
        p_yes <- if (v$severity == "High") 0.75 else 0.55
        acc_center <- if (v$provider == "Doctor") 5.5 else 4.7
        trust_center <- if (v$provider == "Doctor") 5.6 else 4.4
        
        q1 <- rand_yes_no(p_yes)
        q2 <- rand_scale7(center = acc_center, spread = 1.2)
        q3 <- rand_scale7(center = trust_center, spread = 1.3)
        q4 <- rand_scale7(center = 3.5, spread = 1.5)  # Concern about condition
        q5 <- rand_scale7(center = 3.8, spread = 1.4)  # Concern about circumstances
        
        # Timings: started a bit earlier than submitted
        started_at <- current_time + (i - 1) * runif(1, 60, 180)
        submitted_at <- started_at + runif(1, 20, 90)
        
        idx <- idx + 1L
        all_rows[[idx]] <- data.frame(
          participant_id = pid,
          vignette_number = i,
          provider = v$provider,
          severity = v$severity,
          advice_len = v$advice_len,
          q_take_advice = (q1 == "Yes"),
          q_perceived_accuracy = as.integer(q2),
          q_trustworthiness = as.integer(q3),
          q_concern_condition = as.integer(q4),
          q_concern_circumstances = as.integer(q5),
          post_vignette_response = NA_character_,
          started_at = started_at,
          submitted_at = submitted_at,
          stringsAsFactors = FALSE
        )
        current_time <- submitted_at
      }
      
      # === POST-VIGNETTE QUESTION ===
      post_responses <- c(
        "I preferred the doctor because they seemed more knowledgeable and trustworthy.",
        "I liked the AI better because it was more objective and detailed.",
        "Both were helpful in different ways, hard to choose.",
        "The doctor felt more personal and understanding.",
        "The AI was faster and more consistent.",
        "I trust human judgment more for health decisions.",
        "The AI provided more comprehensive information."
      )
      
      idx <- idx + 1L
      all_rows[[idx]] <- data.frame(
        participant_id = pid, vignette_number = 99L, provider = "POST_VIGNETTE",
        severity = "POST_VIGNETTE", advice_len = "POST_VIGNETTE",
        q_take_advice = NA, q_perceived_accuracy = NA_integer_, q_trustworthiness = NA_integer_,
        q_concern_condition = NA_integer_, q_concern_circumstances = NA_integer_,
        post_vignette_response = sample(post_responses, 1),
        started_at = current_time, submitted_at = current_time + runif(1, 30, 120),
        stringsAsFactors = FALSE
      )
    }
    
    # Remove any NULL entries and combine
    all_rows <- all_rows[!sapply(all_rows, is.null)]
    do.call(rbind, all_rows)
  }
  
  # Simulate on click
  observeEvent(input$btn_simulate, {
    n <- as.integer(input$sim_n %||% 0)
    if (is.na(n) || n <= 0) {
      showModal(modalDialog(title = "Invalid number", "Enter a positive integer.", easyClose = TRUE))
      return()
    }
    
    # Show progress notification
    showNotification("Generating simulation data... This may take a moment.", type = "message", duration = 3)
    
    sim <- simulate_participants(n, seed = input$sim_seed)
    rv$sim_rows <- sim
    
    # Count the data for notification
    participants <- length(unique(sim$participant_id))
    total_rows <- nrow(sim)
    
    showNotification(
      sprintf("Simulated %d participants with complete data (%d total rows).", participants, total_rows), 
      type = "message", 
      duration = 5
    )
  })
  
  # Show a more detailed summary
  output$simSummary <- renderUI({
    dat <- rv$sim_rows
    if (is.null(dat) || !NROW(dat)) return(NULL)
    
    # Count different types of data
    vignette_rows <- sum(dat$vignette_number >= 1 & dat$vignette_number <= 4)
    pre_rows <- sum(dat$vignette_number == 0)
    post_rows <- sum(dat$vignette_number == 99)
    participants <- length(unique(dat$participant_id))
    
    # Break down pre-questionnaire data
    demo_rows <- sum(grepl("DEMOGRAPHICS", dat$provider))
    crt_rows <- sum(grepl("CRT", dat$provider))
    gaais_rows <- sum(grepl("GAAIS", dat$provider))
    fwa_rows <- sum(grepl("FWA", dat$provider))
    teique_rows <- sum(grepl("TEIQUE", dat$provider))
    mhlc_rows <- sum(grepl("MHLC", dat$provider))
    
    
  })
  
  # Download simulated CSV
  output$download_sim_csv <- downloadHandler(
    filename = function() {
      paste0("simulated_responses_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      dat <- rv$sim_rows
      if (is.null(dat) || !NROW(dat)) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      # Fix Excel postcode issue on export only
      dat$post_vignette_response <- ifelse(
        grepl("DEMOGRAPHICS_postcode", dat$severity) & 
          grepl("^[0-9]{2}$", dat$post_vignette_response),
        paste0("'", dat$post_vignette_response),
        dat$post_vignette_response
      )
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  # Progress dots (24 micro-steps)
  output$progressUI <- renderUI({
    if (!rv$started) {
      return(div(class = "progress-dots"))
    }
    
    # PRE-QUESTIONNAIRES (before vignettes start)
    # Hide dots on vignette intro (step 13)
    if (rv$pre_step == 13L) {
      return(div(class = "progress-dots"))
    }
    
    if (rv$pre_step <= 12) {
      if (rv$pre_step == 1) {
        return(div(class = "progress-dots")) # No dots for main intro
      } else if (rv$pre_step == 2) {
        current_q <- length(rv$pre_answers$demographics) + 1
        total_q <- 4
      } else if (rv$pre_step == 4) {
        current_q <- length(rv$pre_answers$crt) + 1
        total_q <- 4
      } else if (rv$pre_step == 6) {
        current_q <- length(rv$pre_answers$gaais) + 1
        total_q <- 21
      } else if (rv$pre_step == 8) {
        current_q <- length(rv$pre_answers$fwa) + 1
        total_q <- 2
      } else if (rv$pre_step == 10) {
        current_q <- length(rv$pre_answers$teique) + 1
        total_q <- 30
      } else if (rv$pre_step == 12) {
        current_q <- length(rv$pre_answers$mhlc) + 1
        total_q <- 18
      } else {
        return(div(class = "progress-dots")) # No dots for other intros
      }
      
      dots <- lapply(1:total_q, function(i) {
        cls <- "dot"
        if (current_q > i) cls <- paste(cls, "done")
        else if (current_q == i) cls <- paste(cls, "active")
        div(class = cls)
      })
      return(div(class = "progress-dots", dots))
    }
    
    # Fallback
    return(div(class = "progress-dots"))
    
    
    # VIGNETTES + POST: show a single 33-dot progress bar
    # Map current position into 1..33
    total_dots <- 33L
    current_idx <- 1L
    
    if (rv$current_vignette >= 1 && rv$current_vignette <= 4) {
      # Steps are 1..8 within each vignette; clamp to [1,8]
      s <- max(1L, min(8L, as.integer(rv$vig_step)))
      current_idx <- (rv$current_vignette - 1L) * 8L + s  # 1..32
    } else if (rv$current_vignette == 5L) {
      # Post-vignette question (single dot, the 33rd)
      current_idx <- 33L
    } else {
      # After completion (rv$current_vignette >= 6)
      current_idx <- 33L
    }
    
    dots <- lapply(1:total_dots, function(i) {
      cls <- "dot"
      if (i < current_idx) cls <- paste(cls, "done")
      else if (i == current_idx) cls <- paste(cls, "active")
      div(class = cls)
    })
    div(class = "progress-dots", dots)
  })
  
  
  # Top progress bar
  set_top_progress <- function() {
    if (!rv$started) {
      session$sendCustomMessage("setTopProgress", 0)
      return()
    }
    # Linearise position 1..45
    # Pre: steps 1..13 (13 includes vignette intro)
    # Vignettes: 4 blocks of 8 steps => 32 steps, indices 13+1 .. 13+32 = 14..45-1
    # Post: 1 step (index 45)
    idx <- 0L
    
    if (rv$current_vignette == 0L) {
      # Still in pre (including step 13 vignette intro)
      idx <- max(1L, min(13L, as.integer(rv$pre_step)))
    } else if (rv$current_vignette >= 1L && rv$current_vignette <= 4L) {
      # 12 pre steps completed fully before entering vignettes
      s <- max(1L, min(8L, as.integer(rv$vig_step)))
      idx <- 12L + (rv$current_vignette - 1L) * 8L + s
      # idx spans 13..44
    } else if (rv$current_vignette == 5L) {
      # Post-vignette open question, treat as the 45th "step"
      idx <- 45L
    } else {
      # Completed
      idx <- 45L
    }
    
    pct <- round(idx / 45 * 100)
    session$sendCustomMessage("setTopProgress", pct)
  }
  
  
  # Add this function near the top of your server function:
  calculate_detailed_progress <- function() {
    if (!rv$started) return(0)
    
    if (rv$current_vignette == 0 && rv$pre_step >= 1 && rv$pre_step <= 14) {
      # Pre-questionnaire detailed progress
      base_progress <- 0
      
      # Fixed steps (intros): 1, 3, 5, 7, 9, 11, 13 = 7 steps
      # Variable steps: 2(4q), 4(4q), 6(21q), 8(2q), 10(31q), 12(18q) = 80 questions
      # Total pre steps equivalent: 7 + 80 = 87 "micro-steps"
      
      if (rv$pre_step == 1) {
        return(1)
      } else if (rv$pre_step == 2) {
        # Demographics: 4 questions
        q_completed <- length(rv$pre_answers$demographics)
        return(2 + (q_completed * 4) / 4)  # 2-6
      } else if (rv$pre_step == 3) {
        return(7)
      } else if (rv$pre_step == 4) {
        # CRT: 4 questions
        q_completed <- length(rv$pre_answers$crt)
        return(8 + (q_completed * 4) / 4)  # 8-12
      } else if (rv$pre_step == 5) {
        return(13)
      } else if (rv$pre_step == 6) {
        # GAAIS: 21 questions
        q_completed <- length(rv$pre_answers$gaais)
        return(14 + (q_completed * 21) / 21)  # 14-35
      } else if (rv$pre_step == 7) {
        return(36)
      } else if (rv$pre_step == 8) {
        # FWA: 2 questions
        q_completed <- length(rv$pre_answers$fwa)
        return(37 + (q_completed * 2) / 2)  # 37-39
      } else if (rv$pre_step == 9) {
        return(40)
      } else if (rv$pre_step == 10) {
        # TEIQue: 31 questions (including attention check)
        q_completed <- length(rv$pre_answers$teique)
        return(41 + (q_completed * 31) / 31)  # 41-72
      } else if (rv$pre_step == 11) {
        return(73)
      } else if (rv$pre_step == 12) {
        # MHLC: 18 questions
        q_completed <- length(rv$pre_answers$mhlc)
        return(74 + (q_completed * 18) / 18)  # 74-92
      } else if (rv$pre_step == 13) {
        return(93)  # Vignette intro
      }
    } else if (rv$current_vignette >= 1 && rv$current_vignette <= 4) {
      # Vignettes: start from step 93, add 32 steps for vignettes
      s <- max(1L, min(8L, as.integer(rv$vig_step)))
      return(93 + (rv$current_vignette - 1L) * 8L + s)  # 94-125
    } else if (rv$current_vignette == 5L) {
      return(126)  # Post-vignette
    } else {
      return(126)  # Completed
    }
  }
  
  set_detailed_progress <- function() {
    detailed_step <- calculate_detailed_progress()
    pct <- round(detailed_step / 126 * 100)  # 126 total micro-steps
    session$sendCustomMessage("setTopProgress", pct)
  }
  
  # Pre-questionnaire rendering
  
  render_pre_questionnaire_screen <- function() {
    pre_step <- rv$pre_step
    
    if (pre_step == 1) {
      # Main introduction
      div(id = "screen", class = "screen",
          div(class = "title", "Study Overview"),
          div(class = "desc", "You will complete 6 questionnaires before the main study. For your convenience, you will be shown two types of progress indicators. The bar at the top indicates your progress through the entire study, while the dots beneath it indicate your progress through the current set of questions (not displayed on this page)."),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", "Begin Questionnaires", class = "btn btn-next")
              )
          )
      )
    } else if (pre_step == 2) {
      render_demographics_screen()
    } else if (pre_step == 3) {
      render_intro_screen("Additional Screening", "You will now complete a series of four open-ended questions designed to generate a baseline.")
    } else if (pre_step == 4) {
      render_crt_screen()
    } else if (pre_step == 5) {
      render_intro_screen("General Attitudes toward AI ", "You will now complete questions about your attitudes toward artificial intelligence.")
    } else if (pre_step == 6) {
      render_gaais_screen()
    } else if (pre_step == 7) {
      render_intro_screen("Familiarity with AI", "You will now answer questions about your familiarity with artificial intelligence.")
    } else if (pre_step == 8) {
      render_fwa_screen()
    } else if (pre_step == 9) {
      render_intro_screen("TEIQue", "You will now complete a questionnaire that evaluates your level of trait emotional intelligence.")
    } else if (pre_step == 10) {
      render_teique_screen()
    } else if (pre_step == 11) {
      render_intro_screen("MHLC", "You will now complete questions about health beliefs.")
    } else if (pre_step == 12) {
      render_mhlc_screen()
    } else if (pre_step == 13) {
      # Vignette introduction
      div(id = "screen", class = "screen",
          div(class = "title", "Healthcare Scenarios"),
          div(class = "desc", "You will now be presented with a series of healthcare scenarios. You will be asked to imagine that you have certain symptoms for which you will receive healthcare advice. Please read it carefully. Following each scenario, you will be asked to share your perspectives on the advice provided."),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", "Begin Vignettes", class = "btn btn-next")
              )
          )
      )
    }
  }
  
  render_intro_screen <- function(title, description) {
    div(id = "screen", class = "screen",
        div(class = "title", title),
        div(class = "desc", description),
        div(class = "nav-actions",
            actionButton("btn_back", "Back", class = "btn btn-back"),
            div(class = "nav-actions-right",
                actionButton("btn_next", "Continue", class = "btn btn-next")
            )
        )
    )
  }
  
  # Individual questionnaire renders
  
  render_demographics_screen <- function() {
    current_q <- length(rv$pre_answers$demographics) + 1
    total_q <- 4
    
    progress_pct <- round((current_q - 1) / total_q * 100)
    
    if (current_q <= total_q) {
      
      
      q_names <- names(demographics_questions)
      current_q_name <- q_names[current_q]
      q_data <- demographics_questions[[current_q_name]]
      
      # ADD DEBUG LINE
      # cat("DEBUG: Rendering demographics screen, question", current_q, "type:", q_data$type, "name:", current_q_name, "\n")
      
      div(id = "screen", class = "screen",
          div(class = "title", paste("Demographics", current_q, "of", total_q)),
          div(class = "label", q_data$question),
          
          if (q_data$type == "scale") {
            # Render as clickable scale
            existing <- rv$pre_answers$demographics[[current_q_name]] %||% NULL
            scale_class <- if (current_q_name == "age") "scale-7" else if (current_q_name == "gender") "scale-3" else "scale-6"
            
            div(class = "scale-wrap",
                div(class = paste("scale-group", scale_class), `data-input` = paste0("demo_", current_q_name),
                    lapply(seq_along(q_data$options), function(i) {
                      option_text <- q_data$options[i]
                      div(
                        class = paste("scale-item", if (!is.null(existing) && existing == option_text) "selected"),
                        `data-value` = as.character(i),
                        option_text
                      )
                    })
                )
            )
          } else {
            # Use a unique input ID for this specific question
            input_id <- paste0("demo_text_", current_q_name)  # e.g., "demo_text_postcode"
            
            # cat("DEBUG: Creating textInput with ID:", input_id, "\n")
            
            # ADD AUTOFOCUS TRIGGER HERE
            session$onFlushed(function() {
              session$sendCustomMessage("autoFocusText", TRUE)
            }, once = TRUE)
            
            textInput(input_id, NULL, placeholder = "Enter first two digits (numbers only)")
          }
          ,
          
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", if (current_q == total_q) "Submit Demographics" else "Next", class = "btn btn-next")
              )
          )
      )
    }
  }
  
  render_crt_screen <- function() {
    current_q <- length(rv$pre_answers$crt) + 1
    total_q <- 4
    progress_pct <- round((current_q - 1) / total_q * 100)
    
    if (current_q <= total_q) {
      q_text <- crt_questions[[current_q]]
      
      # Use unique input ID for CRT questions
      input_id <- paste0("crt_text_q", current_q)  # e.g., "crt_text_q1"
      
      # CRT input constraints by question number with character limits
      if (current_q == 1) {  # Q1: one character (digit)
        crt_input_ui <- tagAppendAttributes(
          textInput(input_id, NULL, placeholder = "Enter digits only", width = "150px"),
          maxlength = "1"
        )
      } else if (current_q == 2) {  # Q2: one character (digit)
        crt_input_ui <- tagAppendAttributes(
          textInput(input_id, NULL, placeholder = "Enter digits only", width = "150px"),
          maxlength = "1"
        )
      } else if (current_q == 3) {  # Q3: Emily question - 10 characters
        crt_input_ui <- tagAppendAttributes(
          textInput(input_id, NULL, placeholder = "Enter name", width = "150px"),
          maxlength = "10"
        )
      } else if (current_q == 4) {  # Q4: three characters
        crt_input_ui <- tagAppendAttributes(
          textInput(input_id, NULL, placeholder = "Enter digits only", width = "150px"),
          maxlength = "3"
        )
      }
      
      # ADD AUTOFOCUS TRIGGER HERE
      session$onFlushed(function() {
        session$sendCustomMessage("autoFocusText", TRUE)
      }, once = TRUE)
      
      div(id = "screen", class = "screen",
          div(class = "title", paste("Additional Screening", current_q, "of", total_q)),
          div(class = "label", q_text),
          crt_input_ui,
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", if (current_q == total_q) "Submit Baseline" else "Next", class = "btn btn-next")
              )
          )
      )
    }
  }
  
  render_gaais_screen <- function() {
    current_q <- length(rv$pre_answers$gaais) + 1
    total_q <- 21
    progress_pct <- round((current_q - 1) / total_q * 100)
    
    if (current_q <= total_q) {
      
      
      
      q_text <- gaais_questions[current_q]
      existing <- rv$pre_answers$gaais[[paste0("q", current_q)]] %||% NULL
      
      div(id = "screen", class = "screen",
          div(class = "title", paste("GAAIS", current_q, "of", total_q)),
          div(class = "label", q_text),
          div(class = "scale-wrap",
              div(class = paste("scale-group", "scale-5"), `data-input` = "gaais_q",
                  lapply(1:5, function(i) {
                    labels <- c("Strongly Disagree", "(Somewhat) Disagree", "Neutral", "(Somewhat) Agree", "Strongly Agree")
                    div(
                      class = paste("scale-item", if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                      `data-value` = as.character(i),
                      div(style = "font-size: 14px; font-weight: bold;", as.character(i)),
                      div(style = "font-size: 11px; margin-top: 2px;", labels[i])
                    )
                  })
              )
          ),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", if (current_q == total_q) "Submit GAAIS" else "Next", class = "btn btn-next")
              )
          )
      )
    }
  }
  
  render_fwa_screen <- function() {
    current_q <- length(rv$pre_answers$fwa) + 1
    total_q <- 2
    progress_pct <- round((current_q - 1) / total_q * 100)
    
    if (current_q <= total_q) {
      
      
      q_text <- fwa_questions[current_q]
      
      div(id = "screen", class = "screen",
          div(class = "title", paste("FWA", current_q, "of", total_q)),
          div(class = "label", q_text),
          div(class = "scale-wrap",
              div(class = paste("scale-group", "scale-5"), `data-input` = "fwa_q",
                  lapply(1:5, function(i) {
                    labels <- c("Never", "Rarely", "Monthly", "Weekly", "Daily")
                    div(
                      class = paste("scale-item"),
                      `data-value` = as.character(i),
                      div(style = "font-size: 14px; font-weight: bold;", as.character(i)),
                      div(style = "font-size: 11px; margin-top: 2px;", labels[i])
                    )
                  })
              )
          ),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", if (current_q == total_q) "Submit FWA" else "Next", class = "btn btn-next")
              )
          )
      )
    }
  }
  
  render_teique_screen <- function() {
    current_q <- length(rv$pre_answers$teique) + 1
    total_q <- 31 # plus one for attention check 
    progress_pct <- round((current_q - 1) / total_q * 100)
    
    if (current_q <= total_q) {
      
      
      q_text <- teique_questions[current_q]
      existing <- rv$pre_answers$teique[[paste0("q", current_q)]] %||% NULL
      
      # Check if this is the attention check question (question 31)
      is_attention_check <- (current_q == 31)
      
      # Don't show TEIQue header for the attention check question
      title_text <- if (current_q == 31) {
        paste("Kind Request")
      } else {
        paste("TEIQue", current_q, "of", 30)
      }
      
      div(id = "screen", class = "screen",
          div(class = "title", title_text),
          div(class = "label", q_text),
          div(class = "scale-wrap",
              div(class = paste("scale-group", "scale-7"), `data-input` = "teique_q",
                  lapply(1:7, function(i) {
                    div(
                      class = paste("scale-item", if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                      `data-value` = as.character(i),
                      as.character(i)
                    )
                  })
              ),
              div(class = "scale-captions",
                  div(class = "scale-caption-left", "Completely Disagree"),
                  div(class = "scale-caption-right", "Completely Agree")
              )
          ),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", if (current_q == total_q) "Submit TEIQue" else "Next", class = "btn btn-next")
              )
          )
      )
      
    }
  }
  
  render_mhlc_screen <- function() {
    current_q <- length(rv$pre_answers$mhlc) + 1
    total_q <- 18
    progress_pct <- round((current_q - 1) / total_q * 100)
    
    if (current_q <= total_q) {
      
      
      q_text <- mhlc_questions[current_q]
      existing <- rv$pre_answers$mhlc[[paste0("q", current_q)]] %||% NULL
      
      div(id = "screen", class = "screen",
          div(class = "title", paste("MHLC", current_q, "of", total_q)),
          div(class = "label", q_text),
          div(class = "scale-wrap",
              div(class = paste("scale-group", "scale-6"), `data-input` = "mhlc_q",
                  lapply(1:6, function(i) {
                    div(
                      class = paste("scale-item", if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                      `data-value` = as.character(i),
                      as.character(i)
                    )
                  })
              ),
              div(class = "scale-captions",
                  div(class = "scale-caption-left", "Strongly Disagree"),
                  div(class = "scale-caption-right", "Strongly Agree")
              )
          ),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", if (current_q == total_q) "Submit MHLC" else "Next", class = "btn btn-next")
              )
          )
      )
    }
  }
  
  save_demographics_data <- function() {
    if (length(rv$pre_answers$demographics) != 4) return()
    
    demo_row <- data.frame(
      participant_id = input$participant_id,
      age        = rv$pre_answers$demographics$age,
      gender     = rv$pre_answers$demographics$gender,
      education  = rv$pre_answers$demographics$education,
      postcode   = rv$pre_answers$demographics$postcode,
      demo.started_at   = rv$section_started_at$demographics,
      demo.submitted_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    save_to_table(demo_row, "demographics")
  }
  
  save_crt_data <- function() {
    if (length(rv$pre_answers$crt) != 4) return()
    
    crt_row <- data.frame(
      participant_id = input$participant_id,
      crt.q1 = rv$pre_answers$crt$q1,
      crt.q2 = rv$pre_answers$crt$q2,
      crt.q3 = rv$pre_answers$crt$q3,
      crt.q4 = rv$pre_answers$crt$q4,
      crt.started_at   = rv$section_started_at$crt,
      crt.submitted_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    save_to_table(crt_row, "crt")
  }
  
  
  save_gaais_data <- function() {
    if (length(rv$pre_answers$gaais) != 21) return()
    
    gaais_row <- data.frame(
      participant_id = input$participant_id,
      ga.q1 = rv$pre_answers$gaais$q1, ga.q2 = rv$pre_answers$gaais$q2, ga.q3 = rv$pre_answers$gaais$q3,
      ga.q4 = rv$pre_answers$gaais$q4, ga.q5 = rv$pre_answers$gaais$q5, ga.q6 = rv$pre_answers$gaais$q6,
      ga.q7 = rv$pre_answers$gaais$q7, ga.q8 = rv$pre_answers$gaais$q8, ga.q9 = rv$pre_answers$gaais$q9,
      ga.q10 = rv$pre_answers$gaais$q10, ga.q11 = rv$pre_answers$gaais$q11, 
      ga.q12 = rv$pre_answers$gaais$q12,
      ga.q13.attentioncheck = rv$pre_answers$gaais$q13, ga.q14 = rv$pre_answers$gaais$q14, 
      ga.q15 = rv$pre_answers$gaais$q15,
      ga.q16 = rv$pre_answers$gaais$q16, ga.q17 = rv$pre_answers$gaais$q17, 
      ga.q18 = rv$pre_answers$gaais$q18,
      ga.q19 = rv$pre_answers$gaais$q19, ga.q20 = rv$pre_answers$gaais$q20, 
      ga.q21 = rv$pre_answers$gaais$q21,
      ga.started_at   = rv$section_started_at$gaais,
      ga.submitted_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    save_to_table(gaais_row, "gaais")
  }
  
  save_fwa_data <- function() {
    if (length(rv$pre_answers$fwa) != 2) return()
    
    fwa_row <- data.frame(
      participant_id = input$participant_id,
      fwa.q1 = rv$pre_answers$fwa$q1,
      fwa.q2 = rv$pre_answers$fwa$q2,
      fwa.started_at   = rv$section_started_at$fwa,
      fwa.submitted_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    save_to_table(fwa_row, "fwa")
  }
  
  save_teique_data <- function() {
    if (length(rv$pre_answers$teique) != 31) return()
    
    teique_row <- data.frame(
      participant_id = input$participant_id,
      tq.q1 = rv$pre_answers$teique$q1, tq.q2 = rv$pre_answers$teique$q2, tq.q3 = rv$pre_answers$teique$q3,
      tq.q4 = rv$pre_answers$teique$q4, tq.q5 = rv$pre_answers$teique$q5, tq.q6 = rv$pre_answers$teique$q6,
      tq.q7 = rv$pre_answers$teique$q7, tq.q8 = rv$pre_answers$teique$q8, tq.q9 = rv$pre_answers$teique$q9,
      tq.q10 = rv$pre_answers$teique$q10, tq.q11 = rv$pre_answers$teique$q11, tq.q12 = rv$pre_answers$teique$q12,
      tq.q13 = rv$pre_answers$teique$q13, tq.q14 = rv$pre_answers$teique$q14, tq.q15 = rv$pre_answers$teique$q15,
      tq.q16 = rv$pre_answers$teique$q16, tq.q17 = rv$pre_answers$teique$q17, tq.q18 = rv$pre_answers$teique$q18,
      tq.q19 = rv$pre_answers$teique$q19, tq.q20 = rv$pre_answers$teique$q20, tq.q21 = rv$pre_answers$teique$q21,
      tq.q22 = rv$pre_answers$teique$q22, tq.q23 = rv$pre_answers$teique$q23, tq.q24 = rv$pre_answers$teique$q24,
      tq.q25 = rv$pre_answers$teique$q25, tq.q26 = rv$pre_answers$teique$q26, tq.q27 = rv$pre_answers$teique$q27,
      tq.q28 = rv$pre_answers$teique$q28, tq.q29 = rv$pre_answers$teique$q29, tq.q30 = rv$pre_answers$teique$q30,
      tq.attentioncheck = rv$pre_answers$teique$q31, # new attention check!
      tq.started_at   = rv$section_started_at$teique,
      tq.submitted_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    save_to_table(teique_row, "teique")
  }
  
  save_mhlc_data <- function() {
    if (length(rv$pre_answers$mhlc) != 18) return()
    
    mhlc_row <- data.frame(
      participant_id = input$participant_id,
      mhlc.q1 = rv$pre_answers$mhlc$q1, mhlc.q2 = rv$pre_answers$mhlc$q2, mhlc.q3 = rv$pre_answers$mhlc$q3,
      mhlc.q4 = rv$pre_answers$mhlc$q4, mhlc.q5 = rv$pre_answers$mhlc$q5, mhlc.q6 = rv$pre_answers$mhlc$q6,
      mhlc.q7 = rv$pre_answers$mhlc$q7, mhlc.q8 = rv$pre_answers$mhlc$q8, mhlc.q9 = rv$pre_answers$mhlc$q9,
      mhlc.q10 = rv$pre_answers$mhlc$q10, mhlc.q11 = rv$pre_answers$mhlc$q11, 
      mhlc.q12 = rv$pre_answers$mhlc$q12,
      mhlc.q13 = rv$pre_answers$mhlc$q13, mhlc.q14 = rv$pre_answers$mhlc$q14, 
      mhlc.q15 = rv$pre_answers$mhlc$q15,
      mhlc.q16 = rv$pre_answers$mhlc$q16, mhlc.q17 = rv$pre_answers$mhlc$q17, 
      mhlc.q18 = rv$pre_answers$mhlc$q18,
      mhlc.started_at   = rv$section_started_at$mhlc,
      mhlc.submitted_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    save_to_table(mhlc_row, "mhlc")
  }
  
  
  # Helper function to save to different tables
  save_to_table <- function(row, table_name) {
    if (!is.null(rv$sb_access_token) && nzchar(rv$sb_access_token)) {
      tryCatch({
        # Create custom save function for different table
        url <- paste0(SUPABASE_URL, "/rest/v1/", table_name)
        
        # UTC formatting for POSIXct columns
        for (i in seq_along(row)) {
          if (inherits(row[[i]], "POSIXct")) {
            row[[i]] <- strftime(as.POSIXct(row[[i]], tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          }
        }
        
        headers <- add_headers(
          `apikey` = SUPABASE_KEY,
          `Authorization` = paste("Bearer", rv$sb_access_token),
          `Content-Type` = "application/json",
          `Accept` = "application/json",
          `Prefer` = "return=minimal",
          `Content-Profile` = "public",
          `Accept-Profile` = "public"
        )
        
        json_body <- toJSON(row, auto_unbox = TRUE, na = "null")
        res <- POST(url, headers, body = json_body)
        status <- status_code(res)
        
        if (status >= 200 && status < 300) {
          showNotification(paste(toupper(table_name), "data saved to remote database."), type = "message", duration = 3)
          return(TRUE)
        } else {
          txt <- content(res, as = "text", encoding = "UTF-8")
          stop(sprintf("Supabase insert failed (%s): %s", status, txt))
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Supabase Save Error",
          paste("Error saving", table_name, "data:", e$message),
          easyClose = TRUE
        ))
      })
    }
  }
  
  # helper for postcodes
  
  # Replace your validate_postcode function with this complete mapping:
  validate_postcode <- function(postcode) {
    postcode_map <- list(
      # District 01: Raffles Place, Cecil, Marina, People's Park
      "01" = "Raffles Place / Cecil / Marina / People's Park",
      "02" = "Raffles Place / Cecil / Marina / People's Park",
      "03" = "Raffles Place / Cecil / Marina / People's Park", 
      "04" = "Raffles Place / Cecil / Marina / People's Park",
      "05" = "Raffles Place / Cecil / Marina / People's Park",
      "06" = "Raffles Place / Cecil / Marina / People's Park",
      
      # District 02: Anson, Tanjong Pagar
      "07" = "Anson / Tanjong Pagar",
      "08" = "Anson / Tanjong Pagar",
      
      # District 03: Queenstown, Tiong Bahru
      "14" = "Queenstown / Tiong Bahru",
      "15" = "Queenstown / Tiong Bahru", 
      "16" = "Queenstown / Tiong Bahru",
      
      # District 04: Telok Blangah, Harbourfront
      "09" = "Telok Blangah / Harbourfront",
      "10" = "Telok Blangah / Harbourfront",
      
      # District 05: Pasir Panjang, Hong Leong Garden, Clementi New Town
      "11" = "Pasir Panjang / Hong Leong Garden / Clementi New Town",
      "12" = "Pasir Panjang / Hong Leong Garden / Clementi New Town",
      "13" = "Pasir Panjang / Hong Leong Garden / Clementi New Town",
      
      # District 06: High Street, Beach Road (part)
      "17" = "High Street / Beach Road",
      
      # District 07: Middle Road, Golden Mile
      "18" = "Middle Road / Golden Mile",
      "19" = "Middle Road / Golden Mile",
      
      # District 08: Little India
      "20" = "Little India",
      "21" = "Little India",
      
      # District 09: Orchard, Cairnhill, River Valley
      "22" = "Orchard / Cairnhill / River Valley",
      "23" = "Orchard / Cairnhill / River Valley",
      
      # District 10: Ardmore, Bukit Timah, Holland Road, Tanglin
      "24" = "Ardmore / Bukit Timah / Holland Road / Tanglin",
      "25" = "Ardmore / Bukit Timah / Holland Road / Tanglin",
      "26" = "Ardmore / Bukit Timah / Holland Road / Tanglin",
      "27" = "Ardmore / Bukit Timah / Holland Road / Tanglin",
      
      # District 11: Watten Estate, Novena, Thomson
      "28" = "Watten Estate / Novena / Thomson",
      "29" = "Watten Estate / Novena / Thomson",
      "30" = "Watten Estate / Novena / Thomson",
      
      # District 12: Balestier, Toa Payoh, Serangoon
      "31" = "Balestier / Toa Payoh / Serangoon",
      "32" = "Balestier / Toa Payoh / Serangoon",
      "33" = "Balestier / Toa Payoh / Serangoon",
      
      # District 13: Macpherson, Braddell
      "34" = "Macpherson / Braddell",
      "35" = "Macpherson / Braddell",
      "36" = "Macpherson / Braddell",
      "37" = "Macpherson / Braddell",
      
      # District 14: Geylang, Eunos
      "38" = "Geylang / Eunos",
      "39" = "Geylang / Eunos",
      "40" = "Geylang / Eunos",
      "41" = "Geylang / Eunos",
      
      # District 15: Katong, Joo Chiat, Amber Road
      "42" = "Katong / Joo Chiat / Amber Road",
      "43" = "Katong / Joo Chiat / Amber Road",
      "44" = "Katong / Joo Chiat / Amber Road",
      "45" = "Katong / Joo Chiat / Amber Road",
      
      # District 16: Bedok, Upper East Coast, Eastwood, Kew Drive
      "46" = "Bedok / Upper East Coast / Eastwood / Kew Drive",
      "47" = "Bedok / Upper East Coast / Eastwood / Kew Drive",
      "48" = "Bedok / Upper East Coast / Eastwood / Kew Drive",
      
      # District 17: Loyang, Changi
      "49" = "Loyang / Changi",
      "50" = "Loyang / Changi",
      "81" = "Loyang / Changi",
      
      # District 18: Tampines, Pasir Ris
      "51" = "Tampines / Pasir Ris",
      "52" = "Tampines / Pasir Ris",
      
      # District 19: Serangoon Garden, Hougang, Punggol
      "53" = "Serangoon Garden / Hougang / Punggol",
      "54" = "Serangoon Garden / Hougang / Punggol",
      "55" = "Serangoon Garden / Hougang / Punggol",
      "82" = "Serangoon Garden / Hougang / Punggol",
      
      # District 20: Bishan, Ang Mo Kio
      "56" = "Bishan / Ang Mo Kio",
      "57" = "Bishan / Ang Mo Kio",
      
      # District 21: Upper Bukit Timah, Clementi Park, Ulu Pandan
      "58" = "Upper Bukit Timah / Clementi Park / Ulu Pandan",
      "59" = "Upper Bukit Timah / Clementi Park / Ulu Pandan",
      
      # District 22: Jurong
      "60" = "Jurong",
      "61" = "Jurong",
      "62" = "Jurong",
      "63" = "Jurong",
      "64" = "Jurong",
      
      # District 23: Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang
      "65" = "Hillview / Dairy Farm / Bukit Panjang / Choa Chu Kang",
      "66" = "Hillview / Dairy Farm / Bukit Panjang / Choa Chu Kang",
      "67" = "Hillview / Dairy Farm / Bukit Panjang / Choa Chu Kang",
      "68" = "Hillview / Dairy Farm / Bukit Panjang / Choa Chu Kang",
      
      # District 24: Lim Chu Kang, Tengah
      "69" = "Lim Chu Kang / Tengah",
      "70" = "Lim Chu Kang / Tengah",
      "71" = "Lim Chu Kang / Tengah",
      
      # District 25: Kranji, Woodgrove
      "72" = "Kranji / Woodgrove",
      "73" = "Kranji / Woodgrove",
      
      # District 26: Upper Thomson, Springleaf
      "77" = "Upper Thomson / Springleaf",
      "78" = "Upper Thomson / Springleaf",
      
      # District 27: Yishun, Sembawang
      "75" = "Yishun / Sembawang",
      "76" = "Yishun / Sembawang",
      
      # District 28: Seletar
      "79" = "Seletar",
      "80" = "Seletar"
    )
    
    # Return the neighborhood name if valid, NULL if invalid
    return(postcode_map[[postcode]])
  }
  
  # Add the confirmation handler
  show_postcode_confirmation <- function(postcode, neighborhood) {
    session$sendCustomMessage('showPostcodeConfirmation', list(
      postcode = postcode,
      neighborhood = neighborhood
    ))
  }
  
  handle_pre_questionnaire_step <- function() {
    pre_step <- rv$pre_step
    
    if (pre_step %in% c(1, 3, 5, 7, 9, 11)) {
      # Intro screens - just advance
      rv$pre_step <- rv$pre_step + 1L
      session$sendCustomMessage('setPreStep', list(step = rv$pre_step))
      return()
      
      
    } else if (pre_step == 2) {
      # Demographics
      current_q_num <- length(rv$pre_answers$demographics) + 1
      
      # Demographics started_at when first question appears
      if (current_q_num == 1L && is.na(rv$section_started_at$demographics)) {
        rv$section_started_at$demographics <- Sys.time()
      }
      
      q_names <- names(demographics_questions)
      current_q_name <- q_names[current_q_num]
      q_data <- demographics_questions[[current_q_name]]
      
      if (q_data$type == "scale") {
        answer_index <- as.integer(input[[paste0("demo_", current_q_name)]] %||% "0")
        if (answer_index == 0) {
          # Try to restore from buffer
          buf_val <- rv$popped_answers$demographics[[current_q_name]]
          if (!is.null(buf_val)) {
            rv$pre_answers$demographics[[current_q_name]] <- buf_val
            rv$popped_answers$demographics[[current_q_name]] <- NULL
            answer_restored <- TRUE
          } else {
            showModal(modalDialog(title = "Missing Answer", "Please provide an answer.", easyClose = TRUE))
            return()
          }
        } else {
          answer_text <- q_data$options[answer_index]
          rv$pre_answers$demographics[[current_q_name]] <- answer_text
          rv$pre_q_completed$demographics[current_q_num] <- Sys.time()
          
          rv$popped_answers$demographics[[current_q_name]] <- NULL  # clear buffer
          check_response_patterns()
          
          set_detailed_progress()  
          
        }
      } else {
        input_id <- paste0("demo_text_", current_q_name)
        answer <- trimws(input[[input_id]] %||% "")
        
        if (!nzchar(answer)) {
          buf_val <- rv$popped_answers$demographics[[current_q_name]]
          if (!is.null(buf_val)) {
            rv$pre_answers$demographics[[current_q_name]] <- buf_val
            rv$popped_answers$demographics[[current_q_name]] <- NULL
          } else {
            showModal(modalDialog(title = "Missing Answer", "Please provide an answer.", easyClose = TRUE))
            return()
          }
        } else {
          # Basic format validation
          if (!grepl("^[0-9]{2}$", answer)) {
            showModal(modalDialog(title = "Invalid Postcode", "Please enter exactly 2 digits (numbers only).", easyClose = TRUE))
            return()
          }
          
          # Check if it's a valid postcode
          neighborhood <- validate_postcode(answer)
          if (!is.null(neighborhood)) {
            # Valid postcode - show confirmation
            show_postcode_confirmation(answer, neighborhood)
            return()  # Don't proceed until user confirms
          } else {
            # Invalid postcode - show error
            showModal(modalDialog(
              title = "Unrecognized Postcode", 
              paste("We don't recognize the postcode", answer, "in our system. Please double-check and enter the first two digits of your postcode."),
              easyClose = TRUE
            ))
            return()
          }
        }
      }
      
      
      if (current_q_num < 4) {
        # Clear inputs for next question
        updateTextInput(session, "current_answer", value = "")
        session$sendCustomMessage('resetPreStepInputs', list(step = pre_step, id = paste0('demo_', current_q_name)))
      } else {
        # Complete demographics - save and move to next
        save_demographics_data()
        
        rv$pre_step <- rv$pre_step + 1L
        session$sendCustomMessage('setPreStep', list(step = rv$pre_step))   
        
      }
      
    } else if (pre_step == 4) {
      q_num <- length(rv$pre_answers$crt) + 1
      input_id <- paste0("crt_text_q", q_num)
      
      answer <- trimws(input[[input_id]] %||% "")
      q_name <- paste0("q", q_num)
      
      if (length(rv$pre_answers$crt) == 0L && is.na(rv$section_started_at$crt)) {
        rv$section_started_at$crt <- Sys.time()
      }
      
      # Enhanced validation by question with character limits
      if (!nzchar(answer)) {
        buf_val <- rv$popped_answers$crt[[q_name]]
        if (!is.null(buf_val)) {
          rv$pre_answers$crt[[q_name]] <- buf_val
          rv$popped_answers$crt[[q_name]] <- NULL
        } else {
          showModal(modalDialog(title = "Missing Answer", "Please provide an answer.", easyClose = TRUE))
          return()
        }
      } else {
        # Character and content validation by question
        if (q_num == 1) {  # Q1: single digit
          if (nchar(answer) != 1) {
            showModal(modalDialog(title = "Invalid Length", "Please enter exactly 1 digit.", easyClose = TRUE))
            return()
          }
          if (!grepl("^[0-9]$", answer)) {
            showModal(modalDialog(title = "Invalid Input", "Please enter a single digit (0-9).", easyClose = TRUE))
            return()
          }
        } else if (q_num == 2) {  # Q2: single digit
          if (nchar(answer) != 1) {
            showModal(modalDialog(title = "Invalid Length", "Please enter exactly 1 digit.", easyClose = TRUE))
            return()
          }
          if (!grepl("^[0-9]$", answer)) {
            showModal(modalDialog(title = "Invalid Input", "Please enter a single digit (0-9).", easyClose = TRUE))
            return()
          }
        } else if (q_num == 3) {  # Q3: Emily question - up to 10 characters, letters only
          if (nchar(answer) > 10) {
            showModal(modalDialog(title = "Too Long", "Please enter 10 characters or fewer.", easyClose = TRUE))
            return()
          }
          if (!grepl("^[A-Za-z]+$", answer)) {
            showModal(modalDialog(title = "Invalid Input", "Please enter letters only (no numbers or symbols).", easyClose = TRUE))
            return()
          }
        } else if (q_num == 4) {  # Q4: up to 3 characters, digits only
          if (nchar(answer) > 3) {
            showModal(modalDialog(title = "Too Long", "Please enter 3 characters or fewer.", easyClose = TRUE))
            return()
          }
          if (!grepl("^[0-9]+$", answer)) {
            showModal(modalDialog(title = "Invalid Input", "Please enter digits only.", easyClose = TRUE))
            return()
          }
        }
        
        rv$pre_answers$crt[[q_name]] <- answer
        rv$pre_q_completed$crt[q_num] <- Sys.time()
        rv$popped_answers$crt[[q_name]] <- NULL
        check_response_patterns()
        set_detailed_progress()
      }
      
      if (q_num < 4) {
        updateTextInput(session, "current_answer", value = "")
      } else {
        save_crt_data()
        rv$pre_step <- rv$pre_step + 1L
        session$sendCustomMessage('setPreStep', list(step = rv$pre_step))
      }
    } else if (pre_step == 6) {
      # GAAIS
      answer <- input$gaais_q %||% ""
      q_num <- length(rv$pre_answers$gaais) + 1
      if (length(rv$pre_answers$gaais) == 0L && is.na(rv$section_started_at$gaais)) {
        rv$section_started_at$gaais <- Sys.time()
      }
      q_name <- paste0("q", q_num)
      if (!nzchar(answer)) {
        buf_val <- rv$popped_answers$gaais[[q_name]]
        if (!is.null(buf_val)) {
          rv$pre_answers$gaais[[q_name]] <- as.integer(buf_val)
          rv$popped_answers$gaais[[q_name]] <- NULL
        } else {
          showModal(modalDialog(title = "Missing Answer", "Please select a rating.", easyClose = TRUE))
          return()
        }
      } else {
        rv$pre_answers$gaais[[q_name]] <- as.integer(answer)
        rv$pre_q_completed$gaais[q_num] <- Sys.time()
        
        rv$popped_answers$gaais[[q_name]] <- NULL  # clear buffer
        check_response_patterns()
        
        set_detailed_progress()  
        
      }
      
      
      if (q_num < 21) {
        # Reset scale selection
        session$sendCustomMessage('resetPreStepInputs', list(step = pre_step, id = 'gaais_q'))
      } else {
        save_gaais_data()
        
        rv$pre_step <- rv$pre_step + 1L
        session$sendCustomMessage('setPreStep', list(step = rv$pre_step))   
        
      }
      
    } else if (pre_step == 8) {
      # FWA
      answer <- input$fwa_q %||% ""
      q_num <- length(rv$pre_answers$fwa) + 1
      if (length(rv$pre_answers$fwa) == 0L && is.na(rv$section_started_at$fwa)) {
        rv$section_started_at$fwa <- Sys.time()
      }
      q_name <- paste0("q", q_num)
      if (!nzchar(answer)) {
        buf_val <- rv$popped_answers$fwa[[q_name]]
        if (!is.null(buf_val)) {
          rv$pre_answers$fwa[[q_name]] <- as.integer(buf_val)
          rv$popped_answers$fwa[[q_name]] <- NULL
        } else {
          showModal(modalDialog(title = "Missing Answer", "Please select a rating.", easyClose = TRUE))
          return()
        }
      } else {
        rv$pre_answers$fwa[[q_name]] <- as.integer(answer)
        rv$pre_q_completed$fwa[q_num] <- Sys.time()
        
        rv$popped_answers$fwa[[q_name]] <- NULL  # clear buffer
        check_response_patterns()
        
        set_detailed_progress()  
        
      }
      
      if (q_num < 2) {
        session$sendCustomMessage('resetPreStepInputs', list(step = pre_step, id = 'fwa_q'))
      } else {
        save_fwa_data()
        
        rv$pre_step <- rv$pre_step + 1L
        session$sendCustomMessage('setPreStep', list(step = rv$pre_step))   
        
      }
      
    } else if (pre_step == 10) {
      # TEIQue
      answer <- input$teique_q %||% ""
      q_num <- length(rv$pre_answers$teique) + 1
      if (length(rv$pre_answers$teique) == 0L && is.na(rv$section_started_at$teique)) {
        rv$section_started_at$teique <- Sys.time()
      }
      q_name <- paste0("q", q_num)
      if (!nzchar(answer)) {
        buf_val <- rv$popped_answers$teique[[q_name]]
        if (!is.null(buf_val)) {
          rv$pre_answers$teique[[q_name]] <- as.integer(buf_val)
          rv$popped_answers$teique[[q_name]] <- NULL
        } else {
          showModal(modalDialog(title = "Missing Answer", "Please select a rating.", easyClose = TRUE))
          return()
        }
      } else {
        rv$pre_answers$teique[[q_name]] <- as.integer(answer)
        rv$pre_q_completed$teique[q_num] <- Sys.time()
        
        rv$popped_answers$teique[[q_name]] <- NULL  # clear buffer
        
        # ADD THE ATTENTION CHECK HERE:
        if (q_num == 31 && as.integer(answer) != 1) {
          rv$attention_check_failed <- TRUE
          show_blocking_warning(
            title = "Attention Check Failed",
            message = "The previous question specifically asked you to select 'Completely Disagree' but you selected a different option. Please read all instructions carefully throughout the study.",
            duration = 15
          )
        }
        check_response_patterns()
        
        set_detailed_progress()  
        
      }
      
      
      if (q_num < 31) {
        session$sendCustomMessage('resetPreStepInputs', list(step = pre_step, id = 'teique_q'))
      } else {
        save_teique_data()
        
        rv$pre_step <- rv$pre_step + 1L
        session$sendCustomMessage('setPreStep', list(step = rv$pre_step)) 
        
      }
      
    } else if (pre_step == 12) {
      # MHLC - last questionnaire
      answer <- input$mhlc_q %||% ""
      q_num <- length(rv$pre_answers$mhlc) + 1
      if (length(rv$pre_answers$mhlc) == 0L && is.na(rv$section_started_at$mhlc)) {
        rv$section_started_at$mhlc <- Sys.time()
      }
      q_name <- paste0("q", q_num)
      if (!nzchar(answer)) {
        buf_val <- rv$popped_answers$mhlc[[q_name]]
        if (!is.null(buf_val)) {
          rv$pre_answers$mhlc[[q_name]] <- as.integer(buf_val)
          rv$popped_answers$mhlc[[q_name]] <- NULL
        } else {
          showModal(modalDialog(title = "Missing Answer", "Please select a rating.", easyClose = TRUE))
          return()
        }
      } else {
        rv$pre_answers$mhlc[[q_name]] <- as.integer(answer)
        rv$pre_q_completed$mhlc[q_num] <- Sys.time()
        
        rv$popped_answers$mhlc[[q_name]] <- NULL  # clear buffer
        check_response_patterns()
        
        set_detailed_progress()  
        
      }
      
      
      if (q_num < 18) {
        session$sendCustomMessage('resetPreStepInputs', list(step = pre_step, id = 'mhlc_q'))
      } else {
        save_mhlc_data()
        
        # After MHLC completion, move to vignette intro
        rv$pre_step <- 13L  # Move to vignette intro
        session$sendCustomMessage('setPreStep', list(step = rv$pre_step))    
        
      }
      
    } else if (pre_step == 13) {
      # 1) Generate vignettes with optional seed (only once, when starting vignettes)
      seed_val <- suppressWarnings(as.integer(input$seed))
      rv$vignettes <- generate_balanced_vignettes(
        seed = if (!is.na(seed_val)) seed_val else NULL
      )
      
      # 2) Switch to Vignette 1, Step 1
      rv$current_vignette <- 1L
      rv$vig_step <- 1L
      
      # 3) Record vignette start time (only once)
      if (is.na(rv$vignette_started_at[1])) {
        rv$vignette_started_at[1] <- Sys.time()
      }
      sync_question_vig_step()
      
    }
  }
  
  
  # Data compile for CSV:
  compile_all_data <- function() {
    tryCatch({
      # Start with a copy of vignette data structure
      all_data <- rv$responses
      
      # Get the exact column structure from rv$responses
      template_row <- all_data[1, , drop = FALSE]
      if (nrow(all_data) > 0) {
        template_row[1, ] <- NA  # Clear the values but keep structure
      } else {
        # Create template if no vignette data exists
        template_row <- data.frame(
          participant_id = character(1),
          vignette_number = integer(1),
          provider = character(1),
          severity = character(1),
          advice_len = character(1),
          q_take_advice = logical(1),
          q_perceived_accuracy = integer(1),
          q_trustworthiness = integer(1),
          q_concern_condition = integer(1),
          q_concern_circumstances = integer(1),
          post_vignette_response = character(1),
          started_at = as.POSIXct(character(1)),
          submitted_at = as.POSIXct(character(1)),
          stringsAsFactors = FALSE
        )
      }
      
      # Helper function to create rows with correct structure
      create_pre_row <- function(q_type, q_name, response, started_time) {
        new_row <- template_row
        new_row$participant_id <- input$participant_id
        new_row$vignette_number <- 0L  # Use 0 for pre-questionnaire
        new_row$provider <- paste0("PRE_", toupper(q_type))
        new_row$severity <- paste0(toupper(q_type), "_", q_name)
        new_row$advice_len <- toupper(q_type)
        new_row$q_take_advice <- NA
        new_row$q_perceived_accuracy <- NA_integer_
        new_row$q_trustworthiness <- NA_integer_
        new_row$q_concern_condition <- NA_integer_
        new_row$q_concern_circumstances <- NA_integer_
        new_row$post_vignette_response <- as.character(response)
        new_row$started_at <- started_time
        new_row$submitted_at <- started_time
        return(new_row)
      }
      
      # Add demographics data
      if (length(rv$pre_answers$demographics) > 0) {
        for (q_name in names(rv$pre_answers$demographics)) {
          new_row <- create_pre_row("demographics", q_name, 
                                    rv$pre_answers$demographics[[q_name]], 
                                    rv$section_started_at$demographics)
          all_data <- rbind(all_data, new_row)
        }
      }
      
      # Add CRT data
      if (length(rv$pre_answers$crt) > 0) {
        for (q_name in names(rv$pre_answers$crt)) {
          new_row <- create_pre_row("crt", q_name, 
                                    rv$pre_answers$crt[[q_name]], 
                                    rv$section_started_at$crt)
          all_data <- rbind(all_data, new_row)
        }
      }
      
      # Add GAAIS data
      if (length(rv$pre_answers$gaais) > 0) {
        for (q_name in names(rv$pre_answers$gaais)) {
          new_row <- create_pre_row("gaais", q_name, 
                                    rv$pre_answers$gaais[[q_name]], 
                                    rv$section_started_at$gaais)
          all_data <- rbind(all_data, new_row)
        }
      }
      
      # Add FWA data
      if (length(rv$pre_answers$fwa) > 0) {
        for (q_name in names(rv$pre_answers$fwa)) {
          new_row <- create_pre_row("fwa", q_name, 
                                    rv$pre_answers$fwa[[q_name]], 
                                    rv$section_started_at$fwa)
          all_data <- rbind(all_data, new_row)
        }
      }
      
      # Add TEIQue data (including attention check)
      if (length(rv$pre_answers$teique) > 0) {
        for (q_name in names(rv$pre_answers$teique)) {
          new_row <- create_pre_row("teique", q_name, 
                                    rv$pre_answers$teique[[q_name]], 
                                    rv$section_started_at$teique)
          all_data <- rbind(all_data, new_row)
        }
      }
      
      # Add MHLC data
      if (length(rv$pre_answers$mhlc) > 0) {
        for (q_name in names(rv$pre_answers$mhlc)) {
          new_row <- create_pre_row("mhlc", q_name, 
                                    rv$pre_answers$mhlc[[q_name]], 
                                    rv$section_started_at$mhlc)
          all_data <- rbind(all_data, new_row)
        }
      }
      
      return(all_data)
      
    }, error = function(e) {
      cat("Error in compile_all_data:", e$message, "\n")
      return(rv$responses)  # Return original data if error
    })
  }
  
  # Screen UI
  output$screenUI <- renderUI({
    if (!rv$started) {
      div(id = "screen", class="screen",
          div(class = "title", "Welcome"),
          div(class = "desc",
              "Once you have clicked ‘Start,’ you will begin the questionnaire experience. To navigate, you can click the on-screen buttons or use the keyboard: Hit Enter to continue, Shift+Enter to go back, Y/N for Yes and No, and the number keys for the corresponding selections."
          ),
          tagAppendAttributes(
            textInput("participant_id", NULL, 
                      placeholder = "Participant ID (First and last name)")
          ),
          textInput("seed", NULL, placeholder = "Optional randomisation seed"),
          textInput("sb_email", NULL, placeholder = "Supabase email (optional)"),
          passwordInput("sb_password", NULL, 
                        placeholder = "Supabase password (optional)"),
          div(class = "nav-actions",
              actionButton("btn_sb_login", 
                           "Sign in (Optional)", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_start", "Start", class = "btn btn-next")
              )
          ),
          div(class = "footer", ""),
          # Admin - guarded by a passphrase to reveal controls
          # Clickable bar that reveals the Admin Key + Unlock button
          tags$a(
            class = "admin-toggle collapsed",
            `data-bs-toggle` = "collapse",
            href = "#adminKeyCollapse",
            role = "button",
            `aria-expanded` = "false",
            `aria-controls` = "adminKeyCollapse",
            tags$span(class = "chev", HTML("&#9656;")),  # ►
            "Admin"
          ),
          div(
            id = "adminKeyCollapse",
            class = "collapse admin-guard-panel",
            div(class = "label", "Enter Admin Key"),
            passwordInput("admin_key", NULL, placeholder = "Admin key", width = "260px"),
            actionButton("admin_unlock", "Unlock Admin", class = "btn btn-back")
          ),
          
          # Static tools panel (only shown after unlock)
          uiOutput("adminPanel")
      )
    } 
    
    # Show pre-questionnaire (including pre_step 13 vignette splash) only before vignettes start
    else if (rv$current_vignette == 0 && rv$pre_step >= 1 && rv$pre_step <= 13) {
      render_pre_questionnaire_screen()
    }
    
    
    else if (rv$current_vignette >= 1 && rv$current_vignette <= 4) {
      v <- rv$vignettes[rv$current_vignette, ]
      cond <- condition_text(v$severity)
      prov_text <- provider_text(v$provider)
      adv  <- advice_text(v$severity, v$advice_len)
      bullets <- summary_bullets(v$severity)
      icon <- provider_icon(v$provider)
      
      # Sticky summary appears from step 2 onward
      sticky <- if (rv$vig_step >= 2L) {
        div(class = "sticky-summary",
            div(class = "sticky-summary-title", icon, cond$title),
            tags$ul(class = "sticky-summary-ul",
                    tags$li(bullets[1]),
                    tags$li(bullets[2])
            )
        )
      } else NULL
      
      # Step 1: Symptoms only
      if (rv$vig_step == 1L) {
        div(id = "screen", class = "screen",
            div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4: ", cond$title)),
            div(class = "desc", cond$description),
            div(class = "nav-actions",
                actionButton("btn_back", "Back", class = "btn btn-back"),
                div(class = "nav-actions-right",
                    actionButton("btn_next", "Seek medical advice", class = "btn btn-next")
                )
            )
        )
        
        # Step 2: Reveal provider
      } else if (rv$vig_step == 2L) {
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "Your case has been assigned."),
                    div(class = "desc", prov_text),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next", "Receive advice", class = "btn btn-next")
                        )
                    )
                ),
                div(class = "sticky-aside", sticky)
            )
        )
        
        # Step 3: Reveal advice
      } else if (rv$vig_step == 3L) {
        # Mark advice as "long" when it exceeds a threshold (adjust as needed)
        is_long <- nchar(adv) > 500
        
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "Advice:"),
                    # Use "desc long" when is_long is TRUE, otherwise just "desc"
                    div(class = if (is_long) "desc long" else "desc", adv),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next", "Continue", class = "btn btn-next")
                        )
                    )
                ),
                if (!is_long) div(class = "sticky-aside", sticky)
            )
        )
        
        # Step 4: Q1 Yes/No (Typeform boxes)
      } else if (rv$vig_step == 4L) {
        existing <- rv$answers[[rv$current_vignette]]$q1 %||% NULL
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "I would be likely to take this advice, precisely as stated."),
                    div(class = paste("scale-group", "scale-2"), `data-input` = "q1",
                        div(class = paste("scale-item", if (!is.null(existing) && existing == "Yes") "selected"),
                            `data-value` = "Yes", "Yes"
                        ),
                        div(class = paste("scale-item", if (!is.null(existing) && existing == "No") "selected"),
                            `data-value` = "No", "No"
                        )
                    ),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next", "Next", class = "btn btn-next")
                        )
                    )
                ),
                div(class = "sticky-aside", sticky)
            )
        )
        
        # Step 5: Q2 (1..7) with end captions (Very inaccurate ... Very accurate)
      } else if (rv$vig_step == 5L) {
        existing <- rv$answers[[rv$current_vignette]]$q2 %||% NULL
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "To me, the advice seemed…"),
                    div(class = "scale-wrap",
                        div(class = paste("scale-group", "scale-7"), `data-input` = "q2",
                            lapply(1:7, function(i) {
                              div(
                                class = paste("scale-item",
                                              if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                                `data-value` = as.character(i),
                                as.character(i)
                              )
                            })
                        ),
                        div(class = "scale-captions",
                            div(class = "scale-caption-left",  "Very inaccurate"),
                            div(class = "scale-caption-right", "Very accurate")
                        )
                    ),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next", "Next", class = "btn btn-next")
                        )
                    )
                ),
                div(class = "sticky-aside", sticky)
            )
        )
        
        # Step 6: Q3 (1..7) with end captions (Not trustworthy at all ... Completely trustworthy)
      } else if (rv$vig_step == 6L) {
        existing <- rv$answers[[rv$current_vignette]]$q3 %||% NULL
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "I felt that the advice that I was given was…"),
                    div(class = "scale-wrap",
                        div(class = paste("scale-group", "scale-7"), `data-input` = "q3",
                            lapply(1:7, function(i) {
                              div(
                                class = paste("scale-item",
                                              if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                                `data-value` = as.character(i),
                                as.character(i)
                              )
                            })
                        ),
                        div(class = "scale-captions",
                            div(class = "scale-caption-left",  "Not trustworthy at all"),
                            div(class = "scale-caption-right", "Completely trustworthy")
                        )
                    ),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next", "Next", class = "btn btn-next")
                        )
                    )
                ),
                div(class = "sticky-aside", sticky)
            )
        )
        
        
        # Step 7: Q4 (1..7) Concern provider wouldn't recognize uniqueness of medical condition
      } else if (rv$vig_step == 7L) {
        existing <- rv$answers[[rv$current_vignette]]$q4 %||% NULL
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "In this scenario, how concerned are you that this provider would not recognize the uniqueness of your medical condition?"),
                    div(class = "scale-wrap",
                        div(class = paste("scale-group", "scale-7"), `data-input` = "q4",
                            lapply(1:7, function(i) {
                              div(
                                class = paste("scale-item",
                                              if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                                `data-value` = as.character(i),
                                as.character(i)
                              )
                            })
                        ),
                        div(class = "scale-captions",
                            div(class = "scale-caption-left",  "Not at all"),
                            div(class = "scale-caption-right", "Very much")
                        )
                    ),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next", "Next", class = "btn btn-next")
                        )
                    )
                ),
                div(class = "sticky-aside", sticky)
            )
        )
        
        # Step 8: Q5 (1..7) Concern provider wouldn't recognize unique circumstances
      } else if (rv$vig_step == 8L) {
        existing <- rv$answers[[rv$current_vignette]]$q5 %||% NULL
        div(id = "screen", class = "screen",
            div(class = "screen-content",
                div(class = "main-pane",
                    div(class = "title", paste0("Vignette ", rv$current_vignette, " of 4")),
                    div(class = "label", "In this scenario, how concerned are you that this provider would not recognize your unique circumstances?"),
                    div(class = "scale-wrap",
                        div(class = paste("scale-group", "scale-7"), `data-input` = "q5",
                            lapply(1:7, function(i) {
                              div(
                                class = paste("scale-item",
                                              if (!is.null(existing) && as.character(existing) == as.character(i)) "selected"),
                                `data-value` = as.character(i),
                                as.character(i)
                              )
                            })
                        ),
                        div(class = "scale-captions",
                            div(class = "scale-caption-left",  "Not at all"),
                            div(class = "scale-caption-right", "Very much")
                        )
                    ),
                    div(class = "nav-actions",
                        actionButton("btn_back", "Back", class = "btn btn-back"),
                        div(class = "nav-actions-right",
                            actionButton("btn_next",
                                         if (rv$current_vignette < 4) "Submit & Next Vignette" else "Submit & Finish",
                                         class = "btn btn-next")
                        )
                    )
                ),
                div(class = "sticky-aside", sticky)
            )
        )
      }
    } else if (rv$current_vignette == 5 && rv$vig_step == 1) {
      # Post-vignette question - open-ended
      existing <- rv$post_vignette_answer
      
      # ADD AUTOFOCUS TRIGGER HERE
      session$onFlushed(function() {
        session$sendCustomMessage("autoFocusText", TRUE)
      }, once = TRUE)
      
      div(id = "screen", class = "screen",
          div(class = "title", "Final Question"),
          div(class = "label", "Did you prefer receiving advice from the doctor or the AI overall? If so, why did you prefer one or the other?"),
          textAreaInput(
            inputId = "post_q_text",
            label = NULL,
            value = existing %||% "",
            placeholder = "Type your response here... (400 words maximum)",
            rows = 6,
            width = "100%",
          ),
          div(class = "nav-actions",
              actionButton("btn_back", "Back", class = "btn btn-back"),
              div(class = "nav-actions-right",
                  actionButton("btn_next", "Submit & Finish", class = "btn btn-next")
              )
          )
      )
    } else {
      # Final
      div(id = "screen", class="screen show",
          div(class = "title", "Thank you"),
          div(class = "desc", "You may download your responses or restart."),
          downloadButton("download_csv", "Download Responses (CSV)", class = "btn btn-download"),
          actionButton("restart", "Restart", class = "btn btn-back"),
          div(class = "footer", "Responses are saved remotely on Supabase. Click download to store your own copy.")
      )
    }
  })
  outputOptions(output, "screenUI", suspendWhenHidden = FALSE)
  outputOptions(output, "progressUI", suspendWhenHidden = FALSE)  # optional, keeps dots updating
  
  # Run once after the first render/flush (safe reactive read via isolate)
  session$onFlushed(function() {
    session$sendCustomMessage("enforceNextDisable", isolate(rv$pre_step))
  }, once = TRUE)
  
  # Run once after the first render/flush (safe reactive read via isolate)
  session$onFlushed(function() {
    session$sendCustomMessage("enforceVigNextDisable", isolate(rv$vig_step))
  }, once = TRUE)
  
  # sync question vig steps
  
  sync_question_vig_step <- function() {
    session$sendCustomMessage('setCurrentVigStep', list(step = rv$vig_step))
    
    v <- rv$current_vignette
    s <- rv$vig_step
    
    if (s == 4L) {
      val <- rv$answers[[v]]$q1 %||% NULL
      if (is.null(val) || !nzchar(val)) {
        session$sendCustomMessage('resetVigStepInputs',   list(step = s, id = 'q1'))
      } else {
        session$sendCustomMessage('prefillVigStepInputs', list(step = s, id = 'q1', value = as.character(val)))
      }
    } else if (s == 5L) {
      val <- rv$answers[[v]]$q2 %||% NULL
      if (is.null(val) || is.na(val)) {
        session$sendCustomMessage('resetVigStepInputs',   list(step = s, id = 'q2'))
      } else {
        session$sendCustomMessage('prefillVigStepInputs', list(step = s, id = 'q2', value = as.character(val)))
      }
    } else if (s == 6L) {
      val <- rv$answers[[v]]$q3 %||% NULL
      if (is.null(val) || is.na(val)) {
        session$sendCustomMessage('resetVigStepInputs',   list(step = s, id = 'q3'))
      } else {
        session$sendCustomMessage('prefillVigStepInputs', list(step = s, id = 'q3', value = as.character(val)))
      }
      
    } else if (s == 7L) {  # NEW
      val <- rv$answers[[v]]$q4 %||% NULL
      if (is.null(val) || is.na(val)) {
        session$sendCustomMessage('resetVigStepInputs',   list(step = s, id = 'q4'))
      } else {
        session$sendCustomMessage('prefillVigStepInputs', list(step = s, id = 'q4', value = as.character(val)))
      }
    } else if (s == 8L) {  # NEW
      val <- rv$answers[[v]]$q5 %||% NULL
      if (is.null(val) || is.na(val)) {
        session$sendCustomMessage('resetVigStepInputs',   list(step = s, id = 'q5'))
      } else {
        session$sendCustomMessage('prefillVigStepInputs', list(step = s, id = 'q5', value = as.character(val)))
      }
    } else if (v == 5 && s == 1) {  # Post-vignette question
      val <- rv$post_vignette_answer
      if (is.null(val) || !nzchar(val)) {
        # Clear the text area
        updateTextAreaInput(session, "post_q_text", value = "")
      } else {
        # Pre-fill the text area
        updateTextAreaInput(session, "post_q_text", value = val)
      }
    } else {
      # Non-question steps
      session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
      return()
    }
    
    session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
  }
  
  observeEvent(input$modal_enter_close, {
    removeModal()
  })
  
  # Skip to vignettes (bypass all pre-questionnaires)
  observeEvent(input$btn_skip_pre, {
    if (!isTRUE(rv$admin_unlocked)) return()
    
    # Validate participant ID is entered
    id <- trimws(input$participant_id %||% "")
    if (!nzchar(id)) {
      showModal(modalDialog(
        title = "Missing Participant ID",
        "Please enter a Participant ID first.",
        easyClose = TRUE
      ))
      return()
    }
    
    # SET THE SKIP FLAG
    rv$admin_skip_used <- TRUE
    
    # Generate fake pre-questionnaire data
    rv$pre_answers <- list(
      demographics = list(
        age = "25-34",
        gender = "Male", 
        education = "Bachelor's",
        postcode = "12"
      ),
      crt = list(
        q1 = "2", q2 = "8", q3 = "Emily", q4 = "0"
      ),
      gaais = as.list(setNames(rep(3L, 21), paste0("q", 1:21))),
      fwa = list(q1 = 2L, q2 = 1L),
      teique = as.list(setNames(rep(4L, 31), paste0("q", 1:31))),
      mhlc = as.list(setNames(rep(3L, 18), paste0("q", 1:18)))
    )
    
    # Set fake timestamps
    fake_time <- Sys.time() - 300  # 5 minutes ago
    rv$section_started_at <- list(
      demographics = fake_time,
      crt = fake_time + 60,
      gaais = fake_time + 120,
      fwa = fake_time + 180,
      teique = fake_time + 240,
      mhlc = fake_time + 300
    )
    
    # Generate vignettes
    seed_val <- suppressWarnings(as.integer(input$seed))
    rv$vignettes <- generate_balanced_vignettes(
      seed = if (!is.na(seed_val)) seed_val else NULL
    )
    
    # Skip to vignettes
    rv$started <- TRUE
    rv$current_vignette <- 1L
    rv$vig_step <- 1L
    rv$pre_step <- 13L  # Completed
    
    # Record vignette start time
    rv$vignette_started_at[1] <- Sys.time()
    
    # Update client state
    session$sendCustomMessage('setCurrentVignette', list(vignette = 1))
    session$sendCustomMessage('setCurrentVigStep', list(step = 1))
    session$sendCustomMessage('setPreStep', list(step = 13))
    
    sync_question_vig_step()
    set_detailed_progress()
    
    showNotification("Skipped to Vignette 1 with fake pre-questionnaire data.", 
                     type = "message", duration = 4)
  })
  
  # Skip to post-vignette question
  observeEvent(input$btn_skip_to_post, {
    if (!isTRUE(rv$admin_unlocked)) return()
    
    # Validate participant ID is entered
    id <- trimws(input$participant_id %||% "")
    if (!nzchar(id)) {
      showModal(modalDialog(
        title = "Missing Participant ID", 
        "Please enter a Participant ID first.",
        easyClose = TRUE
      ))
      return()
    }
    
    # SET THE SKIP FLAG
    rv$admin_skip_used <- TRUE
    
    # Generate fake pre-questionnaire data (same as above)
    rv$pre_answers <- list(
      demographics = list(age = "25-34", gender = "Male", education = "Bachelor's", postcode = "12"),
      crt = list(q1 = "2", q2 = "8", q3 = "Emily", q4 = "0"),
      gaais = as.list(setNames(rep(3L, 21), paste0("q", 1:21))),
      fwa = list(q1 = 2L, q2 = 1L),
      teique = as.list(setNames(rep(4L, 31), paste0("q", 1:31))),
      mhlc = as.list(setNames(rep(3L, 18), paste0("q", 1:18)))
    )
    
    # Generate fake vignette data
    seed_val <- suppressWarnings(as.integer(input$seed))
    rv$vignettes <- generate_balanced_vignettes(
      seed = if (!is.na(seed_val)) seed_val else NULL
    )
    
    fake_time <- Sys.time() - 600  # 10 minutes ago
    
    # Generate fake vignette responses
    for (i in 1:4) {
      rv$answers[[i]] <- list(
        q1 = sample(c("Yes", "No"), 1),
        q2 = sample(1:7, 1),
        q3 = sample(1:7, 1), 
        q4 = sample(1:7, 1),
        q5 = sample(1:7, 1)
      )
      
      # Add fake response to database
      v <- rv$vignettes[i, ]
      row <- data.frame(
        participant_id = id,
        vignette_number = i,
        provider = v$provider,
        severity = v$severity,
        advice_len = v$advice_len,
        q_take_advice = rv$answers[[i]]$q1 == "Yes",
        q_perceived_accuracy = as.integer(rv$answers[[i]]$q2),
        q_trustworthiness = as.integer(rv$answers[[i]]$q3),
        q_concern_condition = as.integer(rv$answers[[i]]$q4),
        q_concern_circumstances = as.integer(rv$answers[[i]]$q5),
        post_vignette_response = NA_character_,
        started_at = fake_time + (i-1) * 120,
        submitted_at = fake_time + i * 120,
        stringsAsFactors = FALSE
      )
      rv$responses <- rbind(rv$responses, row)
    }
    
    # Skip to post-vignette question
    rv$started <- TRUE
    rv$current_vignette <- 5L
    rv$vig_step <- 1L
    rv$pre_step <- 13L
    
    # Update client state
    session$sendCustomMessage('setCurrentVignette', list(vignette = 5))
    session$sendCustomMessage('setCurrentVigStep', list(step = 1))
    
    set_detailed_progress()
    
    showNotification("Skipped to post-vignette question with fake data.", 
                     type = "message", duration = 4)
  })
  
  # Supabase login handler
  observeEvent(input$btn_sb_login, {
    email <- trimws(input$sb_email %||% "")
    password <- input$sb_password %||% ""
    if (!nzchar(email) || !nzchar(password)) {
      showModal(modalDialog(
        title = "Missing Credentials",
        "Please enter both email and password to sign in to Supabase.",
        easyClose = TRUE
      ))
      return()
    }
    tryCatch({
      token <- supabase_sign_in(email, password)
      rv$sb_access_token <- token$access_token
      rv$sb_refresh_token <- token$refresh_token
      rv$sb_expires_at    <- Sys.time() + as.numeric(token$expires_in)
      rv$sb_user_email <- email
      showNotification("Signed in to Supabase successfully.", type = "message", duration = 4)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Supabase Sign-in Error",
        paste("Could not sign in:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Navigation: Start Button
  
  observeEvent(input$btn_start, {
    id <- trimws(input$participant_id %||% "")
    seed_input <- trimws(input$seed %||% "")
    email_input <- trimws(input$sb_email %||% "")
    password_input <- input$sb_password %||% ""
    
    # Validate input lengths
    if (nchar(id) > 50) {
      showModal(modalDialog(
        title = "Participant ID Too Long",
        "Participant ID must be 50 characters or fewer.",
        easyClose = TRUE
      ))
      return()
    }
    
    if (nchar(seed_input) > 10) {
      showModal(modalDialog(
        title = "Seed Too Long", 
        "Seed must be 10 characters or fewer.",
        easyClose = TRUE
      ))
      return()
    }
    
    if (nchar(email_input) > 100) {
      showModal(modalDialog(
        title = "Email Too Long",
        "Email must be 100 characters or fewer.", 
        easyClose = TRUE
      ))
      return()
    }
    
    if (nchar(password_input) > 50) {
      showModal(modalDialog(
        title = "Password Too Long",
        "Password must be 50 characters or fewer.",
        easyClose = TRUE
      ))
      return()
    }
    
    
    if (!nzchar(id)) {
      showModal(modalDialog(
        title = "Missing Participant ID",
        "Please enter a Participant ID to begin.",
        footer = tagList(modalButton("Dismiss")),
        easyClose = TRUE
      ))
      return()
    }
    
    # Uniqueness check (as you already have)
    exists <- FALSE
    tryCatch({
      exists <- supabase_pid_exists(id, access_token = rv$sb_access_token)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Connectivity Issue",
        paste("Could not verify Participant ID uniqueness:", e$message),
        easyClose = TRUE
      ))
      return()
    })
    if (isTRUE(exists)) {
      showModal(modalDialog(
        title = "Participant ID Already Used",
        "It appears that you have already participated in the research. Thank you. If you have not, please enter a different Participant ID.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Pre-questionnaires first
    rv$started <- TRUE
    rv$current_vignette <- 0L
    rv$pre_step <- 1L
    
    # INFORM CLIENT OF CURRENT PRE STEP
    session$sendCustomMessage('setPreStep', list(step = rv$pre_step))
    
    
    session$onFlushed(function() {
      session$sendCustomMessage("cardApplySlideIn", NULL)
    }, once = FALSE)
    
    rv$last_dir <- "forward"
    set_detailed_progress()
    session$sendCustomMessage('enforceNextDisable', rv$pre_step)
    
    # Disable interior scrolling when leaving welcome screen
    session$onFlushed(function() {
      session$sendCustomMessage("disableInteriorScrolling", NULL)
    }, once = TRUE)
  })
  
  ## observeEvent(input$btn_start_2, {
  ##   id <- trimws(input$participant_id %||% "")
  ##   seed_val <- suppressWarnings(as.integer(input$seed))
  ##     rv$vignettes <- generate_balanced_vignettes(
  ##  seed = if (!is.na(seed_val)) seed_val else NULL
  ##     )
  ## rv$started <- TRUE
  ## rv$current_vignette <- 1L
  ## rv$vig_step <- 1L
  ##     
  ## session$onFlushed(function() {
  ## session$sendCustomMessage("cardApplySlideIn", NULL)
  ## }, once = FALSE)
  ##     
  ## sync_question_vig_step()
  ## # Record start time for vignette 1 when step 1 is first shown
  ## if (is.na(rv$vignette_started_at[1])) {
  ## rv$vignette_started_at[1] <- Sys.time()
  ## }
  ## rv$last_dir <- "forward"
  ## set_detailed_progress()
  ## session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
  ## })
  
  
  # Navigation: Start / Next
  
  observeEvent(input$btn_next, {
    dir <- "forward"
    
    # NEW SECTION FOR PRE-QUESTIONNAIRES
    if (rv$started && rv$current_vignette == 0 && rv$pre_step >= 1 && rv$pre_step <= 13) {
      handle_pre_questionnaire_step()
      set_detailed_progress()
      session$onFlushed(function() {
        session$sendCustomMessage('cardApplySlideIn', NULL)
      }, once = FALSE)
      return()
    }
    
    # Start flow
    if (!rv$started) {
      session$onFlushed(function() {
        session$sendCustomMessage('focusPID', TRUE)
      }, once = FALSE)
      id <- trimws(input$participant_id %||% "")
      if (!nzchar(id)) {
        showModal(modalDialog(
          title = "Missing Participant ID",
          "Please enter a Participant ID to begin.",
          easyClose = TRUE
        ))
        return()
      }
      
      seed_val <- suppressWarnings(as.integer(input$seed))
      rv$vignettes <- generate_balanced_vignettes(
        seed = if (!is.na(seed_val)) seed_val else NULL
      )
      rv$started <- TRUE
      rv$current_vignette <- 1L
      rv$vig_step <- 1L
      
      session$onFlushed(function() {
        session$sendCustomMessage("cardApplySlideIn", NULL)
      }, once = FALSE)
      
      sync_question_vig_step()
      # Record start time for vignette 1 when step 1 is first shown
      if (is.na(rv$vignette_started_at[1])) {
        rv$vignette_started_at[1] <- Sys.time()
      }
      rv$last_dir <- dir
      set_detailed_progress()
      session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
      
    }
    
    # DEBUG: Always print current state
    # cat("DEBUG: Current vignette:", rv$current_vignette, "VigStep:", rv$vig_step, "\n")
    
    # Handle post-vignette question FIRST - before any other conditions
    if (rv$current_vignette == 5L && rv$vig_step == 1L) {
      #  cat("DEBUG: In post-vignette handler\n")
      post_q <- trimws(input$post_q_text %||% "")
      #  cat("DEBUG: post_q value: '", post_q, "'\n")
      
      if (is.null(post_q) || !nzchar(trimws(post_q))) {
        showModal(modalDialog(title = "Incomplete Response", "Please provide an answer to the final question.", easyClose = TRUE))
        return()
      }
      
      # Word count validation
      word_count <- length(strsplit(post_q, "\\s+")[[1]])
      if (word_count > 400) {
        showModal(modalDialog(
          title = "Response Too Long", 
          paste("Your response is", word_count, "words. Please shorten it to 400 words or fewer."), 
          easyClose = TRUE
        ))
        return()
      }
      
      rv$post_vignette_answer <- post_q
      
      # Create post-vignette row
      post_row <- data.frame(
        participant_id = input$participant_id,
        vignette_number = 99L,
        provider = "POST_VIGNETTE",
        severity = "POST_VIGNETTE", 
        advice_len = "POST_VIGNETTE",
        q_take_advice = NA,
        q_perceived_accuracy = NA,
        q_trustworthiness = NA,
        q_concern_condition = NA,
        q_concern_circumstances = NA,
        post_vignette_response = post_q,
        started_at = Sys.time(),
        submitted_at = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      # DEBUG: Check what's in the row before sending
      cat("DEBUG: post_row contents:\n")
      print(post_row)
      cat("DEBUG: post_vignette_response value:", post_row$post_vignette_response, "\n")
      cat("DEBUG: JSON being sent:", toJSON(post_row, auto_unbox = TRUE, na = "null"), "\n")
      
      # Add to responses
      rv$responses <- rbind(rv$responses, post_row)
      cat("DEBUG: Added to responses, now has", nrow(rv$responses), "rows\n")
      
      # Check if we have access token
      if (is.null(rv$sb_access_token)) {
        cat("DEBUG: sb_access_token is NULL\n")
      } else if (!nzchar(rv$sb_access_token)) {
        cat("DEBUG: sb_access_token is empty string\n")
      } else {
        cat("DEBUG: sb_access_token exists, length:", nchar(rv$sb_access_token), "\n")
      }
      
      # Save to Supabase
      tryCatch({
        cat("DEBUG: About to save post-vignette to Supabase\n")
        ok <- save_to_supabase_auth(post_row, access_token = rv$sb_access_token)
        if (isTRUE(ok)) {
          showNotification("Post-vignette response saved to remote database.", type = "message", duration = 4)
          cat("DEBUG: Post-vignette saved to remote database successfully\n")
        } else {
          cat("DEBUG: Post-vignette Supabase save returned FALSE\n")
        }
      }, error = function(e) {
        cat("DEBUG: Post-vignette Supabase error:", e$message, "\n")
        showModal(modalDialog(
          title = "Supabase Save Error", 
          paste("Error saving post-vignette response:", e$message),
          easyClose = TRUE
        ))
      })
      
      # Move to completion
      rv$current_vignette <- 6L
      rv$vig_step <- 0L
      rv$last_dir <- dir
      
      set_detailed_progress()
      session$onFlushed(function() {
        session$sendCustomMessage('cardApplySlideIn', NULL)
      }, once = FALSE)
      session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
      return()
    }
    
    # Regular vignette progression (steps 1-8)
    # Regular vignette progression (steps 1-8)
    if (rv$current_vignette >= 1L && rv$current_vignette <= 4L) {
      if (rv$vig_step %in% c(1L, 2L, 3L)) {
        rv$vig_step <- rv$vig_step + 1L
        rv$last_dir <- "forward"
        sync_question_vig_step()  # already present
      } else if (rv$vig_step == 4L) {
        q1 <- input$q1 %||% ""
        if (!nzchar(q1)) {
          showModal(modalDialog(title = "Incomplete Response", "Please select Yes or No.", easyClose = TRUE))
          return()
        }
        if (is.null(rv$answers[[rv$current_vignette]])) rv$answers[[rv$current_vignette]] <- list()
        rv$answers[[rv$current_vignette]]$q1 <- q1
        
        rv$vig_step <- 5L
        rv$last_dir <- "forward"
        sync_question_vig_step()  # sends reset/prefill for q2 at step 5
        check_response_patterns() 
        
      } else if (rv$vig_step == 5L) {
        q2 <- input$q2 %||% ""
        if (!nzchar(q2)) {
          showModal(modalDialog(title = "Incomplete Response", "Please select a value 1–7.", easyClose = TRUE))
          return()
        }
        if (is.null(rv$answers[[rv$current_vignette]])) rv$answers[[rv$current_vignette]] <- list()
        rv$answers[[rv$current_vignette]]$q2 <- as.integer(q2)
        
        rv$vig_step <- 6L
        rv$last_dir <- "forward"
        sync_question_vig_step()  # sends reset/prefill for q3 at step 6
        check_response_patterns() 
        
      } else if (rv$vig_step == 6L) {
        q3 <- input$q3 %||% ""
        if (!nzchar(q3)) {
          showModal(modalDialog(title = "Incomplete Response", "Please select a value 1–7.", easyClose = TRUE))
          return()
        }
        if (is.null(rv$answers[[rv$current_vignette]])) rv$answers[[rv$current_vignette]] <- list()
        rv$answers[[rv$current_vignette]]$q3 <- as.integer(q3)
        
        rv$vig_step <- 7L
        rv$last_dir <- "forward"
        sync_question_vig_step()  # sends reset/prefill for q4 at step 7
        check_response_patterns() 
        
      } else if (rv$vig_step == 7L) {
        q4 <- input$q4 %||% ""
        if (!nzchar(q4)) {
          showModal(modalDialog(title = "Incomplete Response", "Please select a value 1–7.", easyClose = TRUE))
          return()
        }
        if (is.null(rv$answers[[rv$current_vignette]])) rv$answers[[rv$current_vignette]] <- list()
        rv$answers[[rv$current_vignette]]$q4 <- as.integer(q4)
        
        rv$vig_step <- 8L
        rv$last_dir <- "forward"
        sync_question_vig_step()  # sends reset/prefill for q5 at step 8
        check_response_patterns() 
        
      } else if (rv$vig_step == 8L) {
        # Last question of vignette
        q5 <- input$q5 %||% ""
        if (!nzchar(q5)) {
          showModal(modalDialog(title = "Incomplete Response", "Please select a value 1–7.", easyClose = TRUE))
          return()
        }
        if (is.null(rv$answers[[rv$current_vignette]])) rv$answers[[rv$current_vignette]] <- list()
        rv$answers[[rv$current_vignette]]$q5 <- as.integer(q5)
        check_response_patterns() 
        
        # Save vignette data
        ans <- rv$answers[[rv$current_vignette]]
        take_bool <- ans$q1 == "Yes"
        v <- rv$vignettes[rv$current_vignette, ]
        now <- Sys.time()
        
        row <- data.frame(
          participant_id        = input$participant_id,
          vignette_number       = rv$current_vignette,
          provider              = v$provider,
          severity              = v$severity,
          advice_len            = v$advice_len,
          q_take_advice         = take_bool,
          q_perceived_accuracy  = as.integer(ans$q2),
          q_trustworthiness     = as.integer(ans$q3),
          q_concern_condition   = as.integer(ans$q4),
          q_concern_circumstances = as.integer(ans$q5),
          post_vignette_response = NA_character_,  
          started_at            = rv$vignette_started_at[rv$current_vignette],
          submitted_at          = now,
          stringsAsFactors      = FALSE
        )
        rv$responses <- rbind(rv$responses, row)
        
        # Save to Supabase
        if (!is.null(rv$sb_access_token) && nzchar(rv$sb_access_token)) {
          tryCatch({
            ok <- save_to_supabase_auth(row, access_token = rv$sb_access_token)
            if (isTRUE(ok)) {
              showNotification(sprintf("Saved vignette %d to remote database.", rv$current_vignette), type = "message", duration = 4)
            }
          }, error = function(e) {
            showModal(modalDialog(title = "Supabase Save Error", paste("Error:", e$message), easyClose = TRUE))
          })
        }
        
        if (rv$current_vignette < 4L) {
          # Next vignette
          rv$current_vignette <- rv$current_vignette + 1L
          rv$vig_step <- 1L
          if (is.na(rv$vignette_started_at[rv$current_vignette])) {
            rv$vignette_started_at[rv$current_vignette] <- Sys.time()
          }
          sync_question_vig_step()  # KEEP THIS: step 1 screen setup
        } else {
          # Go to post-vignette question
          rv$current_vignette <- 5L
          rv$vig_step <- 1L
        }
        rv$last_dir <- dir
      }
    }
    
    # Capture the current step and vignette BEFORE the onFlushed callback
    current_vigstep_val <- rv$vig_step
    current_vignette_val <- rv$current_vignette
    
    set_detailed_progress()
    session$onFlushed(function() {
      session$sendCustomMessage('cardApplySlideIn', NULL)
      # Use the captured values instead of accessing reactive values
      session$sendCustomMessage('setCurrentVigStep', list(step = current_vigstep_val))
      session$sendCustomMessage('setCurrentVignette', list(vignette = current_vignette_val))
    }, once = FALSE)
    session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
  })
  
  # Navigation: Back
  observeEvent(input$btn_back, {
    if (!rv$started) return()
    dir <- "back"
    
    # ONLY block in pre-questionnaires (current_vignette == 0)
    if (rv$current_vignette == 0 && rv$pre_step >= 1 && rv$pre_step <= 12) {
      if (rv$pre_step %in% c(3, 5, 7, 9, 11)) {
        showNotification("Cannot go back to completed questionnaire.", type = "warning", duration = 3)
        return()
      }
    }
    
    
    # Show notification when at the very beginning (vignette 1, step 1)
    if (rv$current_vignette == 1L && rv$vig_step == 1L) {
      showNotification("Already at the first step.", type = "message", duration = 2)
      return()
    }
    
    # HANDLE BACK NAVIGATION WITHIN PRE-QUESTIONNAIRES
    if (rv$current_vignette == 0 && rv$pre_step >= 1 && rv$pre_step <= 12) {
      
      if (rv$pre_step == 2) {
        # Demographics - go back within questions or to intro
        current_q_num <- length(rv$pre_answers$demographics) + 1
        if (current_q_num > 1) {
          last_q   <- names(rv$pre_answers$demographics)[length(rv$pre_answers$demographics)]
          last_val <- rv$pre_answers$demographics[[last_q]]
          
          # Save to buffer
          rv$popped_answers$demographics[[last_q]] <- last_val
          
          
          rv$pre_answers$demographics[[last_q]] <- NULL
          
          q_data   <- demographics_questions[[last_q]]
          pre_step_val <- isolate(rv$pre_step)
          session$onFlushed(function() {
            if (q_data$type == "scale") {
              idx <- match(last_val, q_data$options)
              if (!is.na(idx)) {
                session$sendCustomMessage(
                  'prefillStepInputs',
                  list(step = pre_step_val, id = paste0('demo_', last_q), value = as.character(idx))
                )
              }
            } else {
              updateTextInput(session, "current_answer", value = last_val %||% "")
            }
          }, once = TRUE)
        } else {
          rv$pre_step <- 1L
          session$sendCustomMessage('setPreStep', list(step = rv$pre_step))  
          
        }
        
      } else if (rv$pre_step == 4) {
        current_q_num <- length(rv$pre_answers$crt) + 1
        if (current_q_num > 1) {
          last_q   <- names(rv$pre_answers$crt)[length(rv$pre_answers$crt)]
          last_val <- rv$pre_answers$crt[[last_q]]
          rv$popped_answers$crt[[last_q]] <- last_val
          
          rv$pre_answers$crt[[last_q]] <- NULL
          
          pre_step_val <- isolate(rv$pre_step)
          session$onFlushed(function() {
            updateTextInput(session, "current_answer", value = last_val %||% "")
          }, once = TRUE)
        } else {
          rv$pre_step <- 3L
          session$sendCustomMessage('setPreStep', list(step = rv$pre_step))
          
        }
        
      } else if (rv$pre_step == 6) {
        current_q_num <- length(rv$pre_answers$gaais) + 1
        if (current_q_num > 1) {
          last_q   <- names(rv$pre_answers$gaais)[length(rv$pre_answers$gaais)]
          last_val <- rv$pre_answers$gaais[[last_q]]  # integer 1..5
          
          rv$popped_answers$gaais[[last_q]] <- last_val
          
          rv$pre_answers$gaais[[last_q]] <- NULL
          
          pre_step_val <- isolate(rv$pre_step)
          session$onFlushed(function() {
            if (!is.null(last_val)) {
              session$sendCustomMessage(
                'prefillStepInputs',
                list(step = pre_step_val, id = 'gaais_q', value = as.character(last_val))
              )
            }
          }, once = TRUE)
        } else {
          rv$pre_step <- 5L
          session$sendCustomMessage('setPreStep', list(step = rv$pre_step)) 
          
        }
        
      } else if (rv$pre_step == 8) {
        current_q_num <- length(rv$pre_answers$fwa) + 1
        if (current_q_num > 1) {
          last_q   <- names(rv$pre_answers$fwa)[length(rv$pre_answers$fwa)]
          last_val <- rv$pre_answers$fwa[[last_q]]  # integer 1..5
          rv$popped_answers$fwa[[last_q]] <- last_val
          
          rv$pre_answers$fwa[[last_q]] <- NULL
          
          pre_step_val <- isolate(rv$pre_step)
          session$onFlushed(function() {
            if (!is.null(last_val)) {
              session$sendCustomMessage(
                'prefillStepInputs',
                list(step = pre_step_val, id = 'fwa_q', value = as.character(last_val))
              )
            }
          }, once = TRUE)
        } else {
          rv$pre_step <- 7L
          session$sendCustomMessage('setPreStep', list(step = rv$pre_step)) 
          
        }
        
      } else if (rv$pre_step == 10) {
        current_q_num <- length(rv$pre_answers$teique) + 1
        if (current_q_num > 1) {
          last_q   <- names(rv$pre_answers$teique)[length(rv$pre_answers$teique)]
          last_val <- rv$pre_answers$teique[[last_q]]  # integer 1..7
          rv$popped_answers$teique[[last_q]] <- last_val
          
          rv$pre_answers$teique[[last_q]] <- NULL
          
          pre_step_val <- isolate(rv$pre_step)
          session$onFlushed(function() {
            if (!is.null(last_val)) {
              session$sendCustomMessage(
                'prefillStepInputs',
                list(step = pre_step_val, id = 'teique_q', value = as.character(last_val))
              )
            }
          }, once = TRUE)
        } else {
          rv$pre_step <- 9L
          session$sendCustomMessage('setPreStep', list(step = rv$pre_step)) 
          
        }
        
      } else if (rv$pre_step == 12) {
        current_q_num <- length(rv$pre_answers$mhlc) + 1
        if (current_q_num > 1) {
          last_q   <- names(rv$pre_answers$mhlc)[length(rv$pre_answers$mhlc)]
          last_val <- rv$pre_answers$mhlc[[last_q]]  # integer 1..6
          rv$popped_answers$mhlc[[last_q]] <- last_val
          
          rv$pre_answers$mhlc[[last_q]] <- NULL
          
          pre_step_val <- isolate(rv$pre_step)
          session$onFlushed(function() {
            if (!is.null(last_val)) {
              session$sendCustomMessage(
                'prefillStepInputs',
                list(step = pre_step_val, id = 'mhlc_q', value = as.character(last_val))
              )
            }
          }, once = TRUE)
        } else {
          rv$pre_step <- 11L
          session$sendCustomMessage('setPreStep', list(step = rv$pre_step))  
          
        }
      }
      
      rv$last_dir <- dir
      set_detailed_progress()
      session$onFlushed(function() {
        session$sendCustomMessage("cardApplySlideIn", NULL)
      }, once = FALSE)
      session$sendCustomMessage('enforceNextDisable', rv$pre_step)
      return()
    }
    
    # VIGNETTE BACK NAVIGATION (restored)
    if (rv$current_vignette >= 1L && rv$current_vignette <= 4L) {
      if (rv$vig_step > 1L) {
        rv$vig_step <- rv$vig_step - 1L
      } else if (rv$vig_step == 1L && rv$current_vignette > 1L) {
        rv$current_vignette <- rv$current_vignette - 1L
        rv$vig_step <- 6L
      }
    } else if (rv$current_vignette == 5L && rv$vig_step == 1L) {
      # Go back to last step of last vignette
      rv$current_vignette <- 4L
      rv$vig_step <- 8L
    } else if (rv$current_vignette == 6L) {
      # Go back to post-vignette question
      rv$current_vignette <- 5L
      rv$vig_step <- 1L}
    
    # If we land on step 1 of a vignette, ensure its start time is set (only once)
    if (rv$vig_step == 1L && rv$current_vignette >= 1L && rv$current_vignette <= 4L) {
      if (is.na(rv$vignette_started_at[rv$current_vignette])) {
        rv$vignette_started_at[rv$current_vignette] <- Sys.time()
      }
    }
    
    rv$last_dir <- dir
    set_detailed_progress()
    sync_question_vig_step()
    session$onFlushed(function() {
      session$sendCustomMessage("cardApplySlideIn", NULL)
    }, once = FALSE)
    session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
  })
  
  # CSV download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("complete_responses_",
             gsub("[^A-Za-z0-9_-]", "_", input$participant_id %||% "anon"),
             ".csv")
    },
    content = function(file) {
      tryCatch({
        cat("Starting CSV compilation...\n")
        all_data <- compile_all_data()
        cat("Data compiled, rows:", nrow(all_data), "\n")
        
        if (nrow(all_data) > 0) {
          # Fix Excel postcode issue on export
          all_data$post_vignette_response <- ifelse(
            grepl("DEMOGRAPHICS_postcode", all_data$severity) & 
              grepl("^[0-9]{2}$", all_data$post_vignette_response),
            paste0("'", all_data$post_vignette_response),
            all_data$post_vignette_response
          )
          
          write.csv(all_data, file, row.names = FALSE)
          cat("CSV written successfully\n")
        } else {
          cat("No data to write, creating empty file\n")
          write.csv(data.frame(message = "No data available"), file, row.names = FALSE)
        }
      }, error = function(e) {
        cat("Error in download handler:", e$message, "\n")
        write.csv(data.frame(
          error = e$message,
          vignette_rows = nrow(rv$responses),
          demographics_answers = length(rv$pre_answers$demographics)
        ), file, row.names = FALSE)
      })
    }
  )
  
  # Restart
  observeEvent(input$restart, {
    rv$admin_skip_used <- FALSE
    rv$started <- FALSE
    rv$vignettes <- NULL
    rv$current_vignette <- 0L
    rv$pre_step <- 0L
    rv$vig_step <- 0L
    rv$answers <- vector("list", 4)
    rv$responses <- rv$responses[0, , drop = FALSE]
    rv$last_dir <- "forward"
    rv$sb_access_token <- NULL
    rv$sb_user_email <- NULL
    rv$post_vignette_answer <- NULL
    
    
    # ADD THESE MISSING RESETS:
    rv$pre_answers <- list(
      demographics = list(),
      crt = list(),
      gaais = list(),
      fwa = list(),
      teique = list(),
      mhlc = list()
    )
    
    rv$popped_answers <- list(
      demographics = list(), 
      crt = list(),
      gaais = list(), 
      fwa = list(),
      teique = list(), 
      mhlc = list()
    )
    
    rv$pre_q_completed <- list(
      demographics = numeric(),
      crt = numeric(),
      gaais = numeric(),
      fwa = numeric(),
      teique = numeric(),
      mhlc = numeric()
    )
    
    rv$section_started_at <- list(
      demographics = as.POSIXct(NA_character_),
      crt         = as.POSIXct(NA_character_),
      gaais       = as.POSIXct(NA_character_),
      fwa         = as.POSIXct(NA_character_),
      teique      = as.POSIXct(NA_character_),
      mhlc        = as.POSIXct(NA_character_)
    )
    
    rv$completed_questionnaires <- character()
    rv$vignette_started_at <- as.POSIXct(rep(NA_character_, 4))
    
    # Clear all input values
    updateTextInput(session, "participant_id", value = "")
    updateTextInput(session, "seed", value = "")
    updateTextInput(session, "sb_email", value = "")
    updateTextInput(session, "sb_password", value = "")
    updateTextAreaInput(session, "post_q_text", value = "")
    
    # Reset client-side state
    session$sendCustomMessage("setTopProgress", 0)
    session$sendCustomMessage('setPreStep', list(step = 0))
    session$sendCustomMessage('setCurrentVignette', list(vignette = 0))
    session$sendCustomMessage('setCurrentVigStep', list(step = 0))
    
    # Reset warning tracking
    rv$warning_cooldowns <- list(
      gaais_straightline = as.POSIXct(NA),
      teique_straightline = as.POSIXct(NA),
      mhlc_straightline = as.POSIXct(NA),
      vignette_pattern = as.POSIXct(NA),
      speed_warning = as.POSIXct(NA),
      attention_check = as.POSIXct(NA)
    )
    rv$warning_shown <- list(
      gaais_straightline = FALSE,
      teique_straightline = FALSE,
      mhlc_straightline = FALSE,
      vignette_pattern = FALSE,
      speed_warning = FALSE
    )
    
    session$onFlushed(function() {
      session$sendCustomMessage('focusPID', TRUE)
    }, once = FALSE)
  })
  
  # Initial transition/progress sync
  observe({
    set_detailed_progress()
    session$sendCustomMessage('enforceVigNextDisable', rv$vig_step)
    if (exists("sync_question_vig_step", inherits = FALSE)) sync_question_vig_step()
    if (!rv$started) {
      session$onFlushed(function() {
        session$sendCustomMessage('focusPID', TRUE)
      }, once = FALSE)
    }
  })
  
  
  # attention checks
  check_response_patterns <- function() {
    
    # SKIP ALL PATTERN DETECTION IF ADMIN SKIP WAS USED
    if (isTRUE(rv$admin_skip_used)) {
      return()
    }
    
    # Helper function to check if warning is on cooldown
    is_on_cooldown <- function(warning_type, cooldown_minutes = 10) {
      last_warning <- rv$warning_cooldowns[[warning_type]]
      if (is.na(last_warning)) return(FALSE)
      
      time_since <- as.numeric(difftime(Sys.time(), last_warning, units = "mins"))
      return(time_since < cooldown_minutes)
    }
    
    # Helper function to set cooldown
    set_cooldown <- function(warning_type) {
      rv$warning_cooldowns[[warning_type]] <- Sys.time()
      rv$warning_shown[[warning_type]] <- TRUE
    }
    
    # Check GAAIS for straight-lining (only once per session)
    if (length(rv$pre_answers$gaais) >= 8 && 
        !rv$warning_shown$gaais_straightline && 
        !is_on_cooldown("gaais_straightline")) {
      
      recent_gaais <- tail(unlist(rv$pre_answers$gaais), 8)
      if (length(unique(recent_gaais)) <= 2) {
        show_blocking_warning(
          title = "Response Pattern Detected",
          message = "We've noticed you have been naughty and have been selecting similar responses to recent questions. Each question asks about different aspects of artificial intelligence. Please read each statement carefully before responding, as your genuine opinions help make this research valuable.",
          duration = 5
        )
        set_cooldown("gaais_straightline")
        return()
      }
    }
    
    # Check TEIQue for straight-lining (only once per session)
    if (length(rv$pre_answers$teique) >= 10 && 
        !rv$warning_shown$teique_straightline && 
        !is_on_cooldown("teique_straightline")) {
      
      recent_teique <- tail(unlist(rv$pre_answers$teique), 10)
      if (length(unique(recent_teique)) == 1) {
        show_blocking_warning(
          title = "Identical Responses Detected", 
          message = "We have noticed you have been naughty and have given the same response to the last 10 questions. People typically vary in their emotional intelligence responses. Please take your time to consider how each statement applies to you personally.",
          duration = 5
        )
        set_cooldown("teique_straightline")
        return()
      }
    }
    
    # Check MHLC for straight-lining (only once per session)
    if (length(rv$pre_answers$mhlc) >= 8 && 
        !rv$warning_shown$mhlc_straightline && 
        !is_on_cooldown("mhlc_straightline")) {
      
      recent_mhlc <- tail(unlist(rv$pre_answers$mhlc), 8)
      if (length(unique(recent_mhlc)) <= 2) {
        show_blocking_warning(
          title = "Response Pattern Detected",
          message = "We noticed you have been naughty and have been selecting similar responses repeatedly. Please take a moment to read each health belief statement carefully, as people typically have varied opinions on these topics. Your thoughtful responses are important for our research.",
          duration = 5
        )
        set_cooldown("mhlc_straightline")
        return()
      }
    }
    
    # Check vignette responses for patterns (only once per session)
    if (rv$current_vignette >= 3 && 
        !rv$warning_shown$vignette_pattern && 
        !is_on_cooldown("vignette_pattern")) {
      
      # Check Q2 (accuracy) responses
      q2_responses <- sapply(1:(rv$current_vignette-1), function(v) {
        rv$answers[[v]]$q2
      })
      q2_responses <- q2_responses[!is.null(q2_responses)]
      
      # Check Q3 (trustworthiness) responses  
      q3_responses <- sapply(1:(rv$current_vignette-1), function(v) {
        rv$answers[[v]]$q3
      })
      q3_responses <- q3_responses[!is.null(q3_responses)]
      
      if ((length(q2_responses) >= 3 && length(unique(q2_responses)) == 1) ||
          (length(q3_responses) >= 3 && length(unique(q3_responses)) == 1)) {
        
        show_blocking_warning(
          title = "Consider Each Scenario Independently",
          message = "You've given identical ratings across different healthcare scenarios. Each scenario involves different conditions and providers. Please evaluate each piece of advice on its own merits.",
          duration = 5
        )
        set_cooldown("vignette_pattern")
        return()
      }
    }
    
    # Check for very fast responses (can repeat, but with cooldown)
    if (length(rv$response_times) > 0 && !is_on_cooldown("speed_warning", 5)) {
      recent_times <- tail(unlist(rv$response_times), 5)
      
      # Very fast average
      if (length(recent_times) >= 4 && mean(recent_times) < 1.5) {
        show_blocking_warning(
          title = "Please Slow Down",
          message = "You appear to be responding very quickly (less than 1.5 seconds per question). Quality research depends on thoughtful responses. Please take time to read and consider each question carefully.",
          duration = 5
        )
        set_cooldown("speed_warning")
        return()
      }
      
      # Extremely fast individual responses
      if (length(recent_times) >= 2 && any(recent_times < 0.8)) {
        show_blocking_warning(
          title = "Response Too Fast",
          message = "Some of your recent responses were given in less than a second. Please ensure you're reading each question completely before selecting your answer.",
          duration = 5
        )
        set_cooldown("speed_warning")
        return()
      }
    }
  }
  
  # Add this helper function to your server:
  is_on_cooldown <- function(warning_type, cooldown_minutes = 10) {
    last_warning <- rv$warning_cooldowns[[warning_type]]
    if (is.na(last_warning)) return(FALSE)
    
    time_since <- as.numeric(difftime(Sys.time(), last_warning, units = "mins"))
    return(time_since < cooldown_minutes)
  }
  
  # Handle postcode confirmation
  observeEvent(input$postcode_confirmed, {
    # User confirmed the postcode is correct - proceed to next question
    current_q_num <- length(rv$pre_answers$demographics) + 1
    q_names <- names(demographics_questions)
    current_q_name <- q_names[current_q_num]
    
    # Get the postcode value
    input_id <- paste0("demo_text_", current_q_name)
    postcode_value <- trimws(input[[input_id]] %||% "")
    
    # Save the confirmed postcode
    excel_postcode <- paste0("'", postcode_value)
    rv$pre_answers$demographics[[current_q_name]] <- postcode_value
    
    rv$pre_q_completed$demographics[current_q_num] <- Sys.time()
    rv$popped_answers$demographics[[current_q_name]] <- NULL
    
    # Proceed with normal flow
    if (current_q_num < 4) {
      session$sendCustomMessage('resetPreStepInputs', list(step = rv$pre_step, id = paste0('demo_', current_q_name)))
    } else {
      save_demographics_data()
      rv$pre_step <- rv$pre_step + 1L
      session$sendCustomMessage('setPreStep', list(step = rv$pre_step))
    }
    
    set_detailed_progress()
  })
  
  # Handle postcode rejection  
  observeEvent(input$postcode_rejected, {
    # User wants to correct - just clear the input and let them re-enter
    current_q_num <- length(rv$pre_answers$demographics) + 1
    q_names <- names(demographics_questions)
    current_q_name <- q_names[current_q_num]
    input_id <- paste0("demo_text_", current_q_name)
    
    # Clear the input
    updateTextInput(session, input_id, value = "")
    
    # Focus back on the input
    session$onFlushed(function() {
      session$sendCustomMessage("autoFocusText", TRUE)
    }, once = TRUE)
  })
  
  # Auto-refresh ~5 minutes before expiry
  observe({
    invalidateLater(60 * 1000, session)
    if (!is.null(rv$sb_expires_at) && !is.null(rv$sb_refresh_token)) {
      if (difftime(rv$sb_expires_at, Sys.time(), units = "mins") < 5) {
        try({
          rt <- supabase_refresh_token(rv$sb_refresh_token)
          rv$sb_access_token <- rt$access_token
          rv$sb_expires_at   <- Sys.time() + as.numeric(rt$expires_in)
          message("Machine user token refreshed.")
        }, silent = TRUE)
      }
    }
  })
  
}



shinyApp(ui, server)
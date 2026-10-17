/*
 * SPDX-FileCopyrightText: Atlas Engineer LLC
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * Scheme list shared by the browser process and the CEF subprocess.
 * Names and privilege bits are fixed before CefInitialize. Chromium does not
 * forward a custom switch or environment variable into the network process, but
 * it does forward --user-data-dir. The list is written there as cl-cef-schemes
 * and every process registers it from that file.
 */

#ifndef CL_CEF_SCHEMES_HH
#define CL_CEF_SCHEMES_HH

#include "include/cef_scheme.h"
#include "include/internal/cef_types.h"

#include <cstdlib>
#include <fstream>
#include <string>
#include <vector>

struct cl_cef_scheme_spec {
  std::string name;
  int options;
};

inline std::string& cl_cef_scheme_spec_storage() {
  static std::string spec;
  return spec;
}

inline void cl_cef_note_schemes(const char* spec) {
  if (spec && *spec)
    cl_cef_scheme_spec_storage() = spec;
}

inline void cl_cef_note_schemes_from_argv(int argc, char** argv) {
  const std::string prefix = "--cl-cef-schemes=";
  if (!argv)
    return;
  for (int i = 0; i < argc; ++i) {
    if (!argv[i])
      continue;
    std::string argument(argv[i]);
    if (argument.compare(0, prefix.size(), prefix) == 0)
      cl_cef_scheme_spec_storage() = argument.substr(prefix.size());
  }
}

inline std::string cl_cef_switch_value(int argc, char** argv, const char* name) {
  if (!argv || !name)
    return {};
  const std::string prefix = std::string("--") + name + "=";
  for (int i = 0; i < argc; ++i) {
    if (!argv[i])
      continue;
    std::string argument(argv[i]);
    if (argument.compare(0, prefix.size(), prefix) == 0)
      return argument.substr(prefix.size());
  }
  return {};
}

inline void cl_cef_write_scheme_file(const char* directory) {
  if (!directory || !*directory)
    return;
  std::ofstream out(std::string(directory) + "/cl-cef-schemes", std::ios::trunc);
  if (out)
    out << cl_cef_scheme_spec_storage();
}

inline void cl_cef_note_schemes_from_user_data(int argc, char** argv) {
  std::string directory = cl_cef_switch_value(argc, argv, "user-data-dir");
  if (directory.empty())
    return;
  std::ifstream in(directory + "/cl-cef-schemes");
  std::string spec;
  if (in)
    std::getline(in, spec);
  cl_cef_note_schemes(spec.c_str());
}

inline std::vector<cl_cef_scheme_spec> cl_cef_parse_schemes(const char* spec) {
  std::vector<cl_cef_scheme_spec> schemes;
  if (!spec || !*spec)
    return schemes;
  std::string input(spec);
  std::size_t start = 0;
  while (start < input.size()) {
    std::size_t comma = input.find(',', start);
    if (comma == std::string::npos)
      comma = input.size();
    std::string item = input.substr(start, comma - start);
    std::size_t colon = item.find(':');
    cl_cef_scheme_spec parsed;
    parsed.name = colon == std::string::npos ? item : item.substr(0, colon);
    parsed.options = colon == std::string::npos ? 0 : std::atoi(item.c_str() + colon + 1);
    if (!parsed.name.empty())
      schemes.push_back(parsed);
    start = comma + 1;
  }
  return schemes;
}

inline void cl_cef_register_schemes(CefRawPtr<CefSchemeRegistrar> registrar) {
  if (!registrar)
    return;
  // These names are fixed before any process starts. Registering them later,
  // or only in the browser process, leaves the network process returning
  // ERR_UNKNOWN_URL_SCHEME.
  const int builtin_options = CEF_SCHEME_OPTION_STANDARD | CEF_SCHEME_OPTION_SECURE |
                              CEF_SCHEME_OPTION_CORS_ENABLED | CEF_SCHEME_OPTION_FETCH_ENABLED;
  const char* builtin[] = {"nyxt", "nyxt-resource", "lisp", "gopher", "gemini", nullptr};
  for (const char** name = builtin; *name; ++name)
    registrar->AddCustomScheme(*name, builtin_options);
  const std::string& stored = cl_cef_scheme_spec_storage();
  const char* spec = stored.empty() ? std::getenv("CL_CEF_SCHEMES") : stored.c_str();
  for (const auto& scheme : cl_cef_parse_schemes(spec)) {
    bool known = false;
    for (const char** name = builtin; *name; ++name) {
      if (scheme.name == *name) {
        known = true;
        break;
      }
    }
    if (!known)
      registrar->AddCustomScheme(scheme.name, scheme.options);
  }
}

#endif

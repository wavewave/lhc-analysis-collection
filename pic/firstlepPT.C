{ 
  TFile s100("ADMXQLD111degenMG1000.0MQ1000.0ML50000.0MN100.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(30); 
  h_s100 -> Draw(); 
  h_s100 -> SetTitle("M_{#tilde{g}}=M_{#tilde{q}}=1000 GeV");
  h_s100 -> SetXTitle("p_{T} (l)");
  h_s100 -> SetYTitle("number of events");
  gPad -> SetLogy();

  TLatex l100(190,100,"m_{N}=100"); 
  l100.SetTextColor(30);
  l100.Draw("same"); 


  TFile s300("ADMXQLD111degenMG1000.0MQ1000.0ML50000.0MN300.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(35); 
  h_s300 -> Draw("same"); 
  h_s300 -> SetTitle("M_{#tilde{g}}=M_{#tilde{q}}=1000 GeV");
  h_s300 -> SetXTitle("p_{T} (l)");
  h_s300 -> SetYTitle("number of events");

  TLatex l300(190,100,"m_{N}=300"); 
  l300.SetTextColor(35);
  l300.Draw("same"); 



  TFile s500("ADMXQLD111degenMG1000.0MQ1000.0ML50000.0MN500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(38); 
  h_s500 -> Draw("same"); 
  h_s500 -> SetTitle("M_{#tilde{g}}=M_{#tilde{q}}=1000 GeV");
  h_s500 -> SetXTitle("p_{T} (l)");
  h_s500 -> SetYTitle("number of events");

  TLatex l500(190,100,"m_{N}=500"); 
  l500.SetTextColor(38);
  l500.Draw("same"); 



  
}

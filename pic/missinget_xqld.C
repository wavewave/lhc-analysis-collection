{ 
  gStyle->SetOptStat(0);  

  TFile l100("ADMXQLD111degenMG1000.0MQ1000.0ML50000.0MN100.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root");
  TH1F* h_l100 = (TH1F*) l100.Get("2jet"); 
  h_l100 -> SetLineColor(30); 
  h_l100 -> Draw(); 
  h_l100 -> SetTitle("M_{#tilde{g}}=M_{#tilde{q}}=1000 GeV");
  h_l100 -> SetXTitle("E_{T}^{miss}");
  h_l100 -> SetYTitle("Number of Events");  


  TFile s100("SimplifiedSUSYMN100.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 

  TLatex l1(150,100,"m_{N}=100");
  l1.SetTextColor(30);  
  TLatex l2(600,65,"m_{N}=100"); 
  l2.SetTextColor(42);
  l1.Draw("same"); 
  l2.Draw("same"); 




  TFile l300("ADMXQLD111degenMG1000.0MQ1000.0ML50000.0MN300.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root");
  TH1F* h_l300 = (TH1F*) l300.Get("2jet"); 
  h_l300 -> SetLineColor(35); 
  h_l300 -> Draw("same"); 
  h_l300 -> SetTitle("Missing ET, mneut=300 GeV, mgluino=msquark=1000 GeV");
 
  TFile s300("SimplifiedSUSYMN300.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l3(170,100,"m_{N}=300"); 
  l3.SetTextColor(35); 
  TLatex l4(550,80,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l3.Draw("same"); 
  l4.Draw("same"); 





  TFile l500("ADMXQLD111degenMG1000.0MQ1000.0ML50000.0MN500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root");
  TH1F* h_l500 = (TH1F*) l500.Get("2jet"); 
  h_l500 -> SetLineColor(38); 
  h_l500 -> Draw("same"); 
  h_l500 -> SetTitle("Missing ET, mneut=500 GeV, mgluino=msquark=1000 GeV");
 
  TFile s500("SimplifiedSUSYMN500.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw("same"); 

  TLatex l5(190,100,"m_{N}=500"); 
  l5.SetTextColor(38);
  TLatex l6(500,95,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l5.Draw("same"); 
  l6.Draw("same"); 


  gPad->Update();
  Double_t x1 = gPad->GetX1(); 
  Double_t x2 = gPad->GetX2();
  Double_t y1 = gPad->GetY1(); 
  Double_t y2 = gPad->GetY2(); 
  Double_t xr = x2-x1; 
  Double_t yr = y2-y1; 

  TBox b1(xr*0.7+x1,yr*0.82+y1,xr*0.72+x1,yr*0.85+y1); 
  b1.SetFillColor(42); 
  b1.Draw("same"); 

  TBox b2(xr*0.73+x1,yr*0.82+y1,xr*0.75+x1,yr*0.85+y1); 
  b2.SetFillColor(44); 
  b2.Draw("same"); 

  TBox b3(xr*0.76+x1,yr*0.82+y1,xr*0.78+x1,yr*0.85+y1); 
  b3.SetFillColor(46); 
  b3.Draw("same"); 
 
  TLatex lsim(xr*0.8+x1,yr*0.82+y1,"Sim0"); 
  lsim.SetTextColor(46); 
  lsim.Draw("same"); 

  TBox b4(xr*0.7+x1,yr*0.75+y1,xr*0.72+x1,yr*0.78+y1); 
  b4.SetFillColor(30); 
  b4.Draw("same"); 

  TBox b5(xr*0.73+x1,yr*0.75+y1,xr*0.75+x1,yr*0.78+y1); 
  b5.SetFillColor(35); 
  b5.Draw("same"); 

  TBox b6(xr*0.76+x1,yr*0.75+y1,xr*0.78+x1,yr*0.78+y1); 
  b6.SetFillColor(38); 
  b6.Draw("same"); 
  




  TLatex lqld(xr*0.8+x1,yr*0.75+y1,"QLD"); 
  lqld.SetTextColor(38); 
  lqld.Draw("same"); 



}
